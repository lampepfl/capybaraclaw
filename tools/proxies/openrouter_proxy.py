#!/usr/bin/env python3
"""
Logging HTTPS proxy for OpenRouter traffic.

Listens on :11437, forwards every request to https://openrouter.ai/api/v1,
and dumps both the request body and response body to stdout + a log file.

Use to diagnose tool-call / message-shape issues between capybaraclaw and
OpenRouter. Forwards the Authorization header transparently.

Defaults (override with env vars):
    PORT=11437                      proxy listen port
    UPSTREAM=https://openrouter.ai/api/v1
                                    OpenRouter base URL
    LOG_PATH=/tmp/openrouterproxy.log

Point capybaraclaw at the proxy by exporting:
    OPENROUTER_BASE_URL=http://localhost:11437

before starting sbt. The OPENROUTER_API_KEY env var still goes to capybaraclaw
as usual — the proxy forwards the Authorization header it builds.

Stop with Ctrl-C.
"""
from __future__ import annotations

import json
import os
import sys
import time
import urllib.error
import urllib.request
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

UPSTREAM = os.environ.get("UPSTREAM", "https://openrouter.ai/api/v1").rstrip("/")
LISTEN_PORT = int(os.environ.get("PORT", "11437"))
LOG_PATH = os.environ.get("LOG_PATH", "/tmp/openrouterproxy.log")

_log_file = open(LOG_PATH, "a", buffering=1, encoding="utf-8")  # line-buffered


def log(msg: str = "") -> None:
    print(msg, flush=True)
    _log_file.write(msg + "\n")


def _pretty(blob: bytes, *, max_len: int = 200_000) -> str:
    if not blob:
        return "(empty body)"
    text = blob.decode("utf-8", errors="replace")
    try:
        parsed = json.loads(text)
        out = json.dumps(parsed, indent=2, ensure_ascii=False)
        return out if len(out) <= max_len else out[:max_len] + f"\n... [truncated, total {len(out)} chars]"
    except json.JSONDecodeError:
        pass
    lines = text.splitlines()
    parsed_lines: list[str] = []
    fail = False
    for line in lines:
        s = line.strip()
        if not s:
            continue
        # OpenRouter / OpenAI SSE: lines like "data: {...}" or "data: [DONE]".
        payload = s[len("data: "):] if s.startswith("data: ") else s
        if payload == "[DONE]":
            parsed_lines.append("[DONE]")
            continue
        try:
            parsed_lines.append(json.dumps(json.loads(payload), ensure_ascii=False))
        except json.JSONDecodeError:
            fail = True
            break
    if not fail and parsed_lines:
        joined = "\n".join(parsed_lines)
        return joined if len(joined) <= max_len else joined[:max_len] + f"\n... [truncated, total {len(joined)} chars]"
    return text if len(text) <= max_len else text[:max_len] + f"\n... [truncated, total {len(text)} chars]"


def _redact_authorization(headers: dict[str, str]) -> dict[str, str]:
    redacted = dict(headers)
    if "Authorization" in redacted:
        v = redacted["Authorization"]
        redacted["Authorization"] = v[:14] + "…" + v[-4:] if len(v) > 20 else "<short>"
    return redacted


class Proxy(BaseHTTPRequestHandler):
    def _proxy(self, method: str) -> None:
        length = int(self.headers.get("Content-Length", "0"))
        req_body = self.rfile.read(length) if length else b""

        ts = time.strftime("%H:%M:%S")
        log("")
        log(f"{'─' * 80}  [{ts}]")
        log(f"→ {method} {self.path}")

        forwarded_headers = {
            k: v for k, v in self.headers.items()
            if k.lower() not in ("host", "content-length", "connection", "accept-encoding")
        }
        log(f"  headers: {json.dumps(_redact_authorization(forwarded_headers))}")
        if req_body:
            log(_pretty(req_body))

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
        return


def main() -> None:
    server = ThreadingHTTPServer(("127.0.0.1", LISTEN_PORT), Proxy)
    log(f"OpenRouter proxy listening on :{LISTEN_PORT}, forwarding to {UPSTREAM}")
    log(f"Logging to {LOG_PATH}")
    log(
        "Point capybaraclaw at the proxy with:\n"
        f"    OPENROUTER_BASE_URL=http://localhost:{LISTEN_PORT}"
    )
    log("Stop with Ctrl-C. Override defaults with PORT / UPSTREAM / LOG_PATH env vars.")
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        log("\nShutting down.")
        server.server_close()
        _log_file.close()


if __name__ == "__main__":
    main()
