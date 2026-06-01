// Compensation Review preamble. Capabilities preamble (loaded first via the
// `requires` declaration) has already imported `safemode.lib.{*, given}`,
// providing Classified, FileSystem, PrivacyBudget, and the host-scoped
// IOCapability / FileSystem givens. This plugin brings the comp-review
// domain symbols into scope.
import safemode.compreview.*
import safemode.compreview.CompReview.{loadCompReview, *}
