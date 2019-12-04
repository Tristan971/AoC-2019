#!/bin/bash

day=$1

echo "Creating exe for day $day"

dune_name="Day$day"

dune init exe "$dune_name" --public="$dune_name" "$dune_name" --libs Commons batteries
