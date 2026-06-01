#!/usr/bin/env python3
"""
Logging HTTP proxy for Ollama traffic.

Listens on :11435 (capybaraclaw's configured base_url), forwards every
request to the real Ollama on :11434, and dumps both the request body and
the response body to stdout. Use to diagnose tool-call / message-shape
issues between capybaraclaw and the model.

Override the upstream with UPSTREAM env var if Ollama runs elsewhere:
    UPSTREAM=http://localhost:11500 python3 /tmp/ollamaproxy.py

Stop with Ctrl-C.
"""
from __future__ import annotations

import json
import os
import socketserver
import sys
import time
import urllib.error
import urllib.request
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

UPSTREAM = os.environ.get("UPSTREAM", "http://localhost:11434").rstrip("/")
LISTEN_PORT = int(os.environ.get("PORT", "11435"))
LOG_PATH = os.environ.get("LOG_PATH", "/tmp/ollamaproxy.log")

_log_file = open(LOG_PATH, "a", buffering=1, encoding="utf-8")  # line-buffered


def log(msg: str = "") -> None:
    """Write to both stdout (live) and the configured log file."""
    print(msg, flush=True)
    _log_file.write(msg + "\n")


def _pretty(blob: bytes, *, max_len: int = 200_000) -> str:
    if not blob:
        return "(empty body)"
    text = blob.decode("utf-8", errors="replace")
    # Try single JSON object first.
    try:
        parsed = json.loads(text)
        out = json.dumps(parsed, indent=2, ensure_ascii=False)
        return out if len(out) <= max_len else out[:max_len] + f"\n... [truncated, total {len(out)} chars]"
    except json.JSONDecodeError:
        pass
    # Try NDJSON (one JSON object per line, e.g. Ollama streaming).
    lines = text.splitlines()
    parsed_lines: list[str] = []
    fail = False
    for line in lines:
        s = line.strip()
        if not s:
            continue
        try:
            parsed_lines.append(json.dumps(json.loads(s), ensure_ascii=False))
        except json.JSONDecodeError:
            fail = True
            break
    if not fail and parsed_lines:
        joined = "\n".join(parsed_lines)
        return joined if len(joined) <= max_len else joined[:max_len] + f"\n... [truncated, total {len(joined)} chars]"
    return text if len(text) <= max_len else text[:max_len] + f"\n... [truncated, total {len(text)} chars]"


class Proxy(BaseHTTPRequestHandler):
    def _proxy(self, method: str) -> None:
        length = int(self.headers.get("Content-Length", "0"))
        req_body = self.rfile.read(length) if length else b""

        ts = time.strftime("%H:%M:%S")
        log("")
        log(f"{'─' * 80}  [{ts}]")
        log(f"→ {method} {self.path}")
        if req_body:
            log(_pretty(req_body))

        forwarded_headers = {
            k: v for k, v in self.headers.items()
            if k.lower() not in ("host", "content-length", "connection", "accept-encoding")
        }
        req = urllib.request.Request(
            UPSTREAM + self.path,
            data=req_body or None,
            method=method,
            headers=forwarded_headers,
        )

        try:
            with urllib.request.urlopen(req, timeout=600) as resp:
                status = resp.status
                resp_headers = list(resp.getheaders())
                resp_body = resp.read()
        except urllib.error.HTTPError as e:
            status = e.code
            resp_headers = list(e.headers.items())
            resp_body = e.read()
        except Exception as e:
            log(f"← PROXY ERROR: {e!r}")
            self.send_response(502)
            self.send_header("Content-Type", "text/plain")
            self.end_headers()
            self.wfile.write(f"proxy error: {e!r}".encode("utf-8"))
            return

        log(f"← {status}")
        if resp_body:
            log(_pretty(resp_body))

        self.send_response(status)
        for k, v in resp_headers:
            if k.lower() in ("transfer-encoding", "content-length", "connection", "content-encoding"):
                continue
            self.send_header(k, v)
        self.send_header("Content-Length", str(len(resp_body)))
        self.end_headers()
        self.wfile.write(resp_body)

    def do_GET(self) -> None:    self._proxy("GET")
    def do_POST(self) -> None:   self._proxy("POST")
    def do_PUT(self) -> None:    self._proxy("PUT")
    def do_DELETE(self) -> None: self._proxy("DELETE")
    def do_PATCH(self) -> None:  self._proxy("PATCH")

    def log_message(self, fmt: str, *args) -> None:
        # Silence default access log; the structured prints above are enough.
        return


def main() -> None:
    server = ThreadingHTTPServer(("127.0.0.1", LISTEN_PORT), Proxy)
    log(f"Ollama proxy listening on :{LISTEN_PORT}, forwarding to {UPSTREAM}")
    log(f"Logging to {LOG_PATH}")
    log("Stop with Ctrl-C. Override defaults with PORT / UPSTREAM / LOG_PATH env vars.")
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        log("\nShutting down.")
        server.server_close()
        _log_file.close()


if __name__ == "__main__":
    main()
