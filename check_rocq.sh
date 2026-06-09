#!/usr/bin/env sh
set -eu
cd "$(dirname "$0")/rocq"
rocq compile CalendarPuzzle.v
rocq compile DateEnumeration.v
rocq compile GeneratedSolutions.v
rocq compile AllDatesProof.v
rocq compile Extract.v
