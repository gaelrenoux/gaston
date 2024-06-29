#!/bin/bash


hours=$1
shift 1

for cid in "$@"
do
  echo "======================================================================="
  echo "Benchmarking version: $cid"
  echo ""
  git co $cid
  sbt "test-slow:runMain fr.renoux.gaston.benchmarks.LongRunnerBenchmark $hours"
  echo "======================================================================="
done
