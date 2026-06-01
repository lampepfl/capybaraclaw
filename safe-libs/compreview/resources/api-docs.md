# Safemode Compensation Review

Domain plugin for analysing an HR compensation spreadsheet without leaking
sensitive pay data through the agent. Loads a `.xlsx` payroll table with this
schema (columns prefixed with `*` are sealed at load time):

```
Name | Department | Level | Location | TenureYears | *Salary | *Bonus | *PerfRating
```

Relative paths resolve against the host workdir (`safemode.workdir` JVM
property). For the demo workdir the file is `payroll.xlsx`.

## Loading

```scala
@assumeSafe def loadCompReview(path: String): CompReview
@assumeSafe def loadCompReview(path: String, budget: PrivacyBudget): CompReview
```

The first overload uses `PrivacyBudget(1.0)`. The budget is plumbed through
the resulting `CompReview` and reserved for future differentially-private
aggregates (out of scope today).

## Public aggregates

These return plain values; the *shape* of the company (department mix, tenure
buckets) is not sensitive.

```scala
def headcountByDept: Map[String, Int]
def tenureBuckets: Map[String, Int]       // "0-2y" | "3-5y" | "6-10y" | "11+y"
```

## Classified aggregates

Every value below is derived from sealed pay data and returns `Classified[…]`.
The agent cannot project them to a public value; surface results through
`writePrivateAnswer`, `writeConfidential`, or `summarize` / `redact` from
the private-llm plugin.

```scala
@assumeSafe def avgSalaryByDept: Classified[Map[String, Double]]
```
Average **total comp** (salary + bonus) per department.

```scala
@assumeSafe def compRatio(level: String, bandMidpoint: Double):
    Classified[Map[String, Double]]
```
Per-employee comp ratio against a band midpoint, filtered to one level.
Useful for spotting under- or over-priced individuals within a band.

```scala
@assumeSafe def outliers(zScore: Double):
    Classified[List[(Employee, Double)]]
```
Employees whose total comp is at least `zScore` standard deviations from the
company mean, paired with their signed z-score. Sorted by `|z|` desc.

```scala
@assumeSafe def bandViolations(bands: Map[String, (Double, Double)]):
    Classified[List[Employee]]
```
Employees whose total comp falls outside the `(low, high)` band declared for
their level. Levels missing from `bands` are skipped.

```scala
@assumeSafe def payDistribution: Classified[List[Double]]
```
Raw total-comp values across the whole company. Useful as input to
`summarize` or `redact` for histogram-style descriptions.

## Idiomatic pattern

```scala
val payroll = loadCompReview("payroll.xlsx")
val violations = payroll.bandViolations(Map(
  "L3" -> (80000.0, 120000.0),
  "L4" -> (110000.0, 160000.0),
  "L5" -> (150000.0, 220000.0),
))
val description = summarize(violations.map(_.toString), maxWords = 80)
writePrivateAnswer(description)
```

`headcountByDept` and `tenureBuckets` can be surfaced directly:

```scala
println(s"Departments: ${payroll.headcountByDept}")
println(s"Tenure mix:  ${payroll.tenureBuckets}")
```
