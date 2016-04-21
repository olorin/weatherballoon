# weatherballoon - Ambiata interview

# Description

This is a solution to the "Weather Observations" coding exercise.

## Generation

The `weatherballoon-generate` program generates an arbitrary number of
randomly-generated records with no specific ordering. The records it
generates can be invalid (have an empty 'observatory' field).

## Reporting

The `weatherballoon-report` program reads in an arbitrary number of
records and (after discarding invalid records) writes out the records
sorted by time and normalized to the requested units, optionally
displaying summary statistics of the data processed.

If a record is invalid (unable to be parsed), the program
reports that parsing failed and discards the record; it was assumed that
partial records could not be meaningfully interpreted and so no attempt
was made to do so. Additionally, the program makes no attempt to detect
physically-impossible conditions in well-formatted records, e.g.,
negative temperatures in Kelvin.

The 'total distance travelled' value was assumed to mean the sum of the
distances travelled between observations (as opposed to the distance
between the last and first observation). Additionally, the balloon is
assumed to have taken the most direct path between observation points;
the program will underestimate distance travelled if this is not the
case.

It's not quite constant-space, requiring memory linear in the total number of
unique observatory names; this was deemed acceptable as the number of
observatories is likely to be far lower than total number of
observations. If the program needed to run in a very-low-memory
environment, or the number of observatories extremely large, the
sighting counts could be stored in a simple database rather than an
in-memory map (with an expected performance hit).

It was assumed that the program would be run on a unixlike system with
GNU coreutils or equivalent (for `sort`); implementing a custom
constant-space sort was considered, but deemed not worth the additional
complexity. If there was a requirement to avoid the dependency, a
standard external merge sort could be used.

# Usage

## Dependencies

Tested on a Linux system with GHC 7.8.3. Requires GNU `sort` (or
analogous) for the external sort.

## Building

```
cabal sandbox init
cabal install --only-dependencies --enable-tests
cabal build
```

## Running

Generate test data:

```
./dist/build/weatherballoon-generate/weatherballoon-generate <number of entries> > data.txt
```

Process it, normalize to Fahrenheit/Miles and report temperature mean
and total distance:

```
./dist/build/weatherballoon-report/weatherballoon-report --report-mean --report-distance -t Fahrenheit -d Miles < data.txt
```

`weatherballoon-report -h` will give you the options for unit
adjustments and reporting. The program uses Metres/Kelvin for output
unless otherwise specified.
