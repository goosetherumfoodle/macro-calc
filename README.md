# [nutrition][]

Requires two yaml files. One with target macronutrient goals, and the
other with a list of foods. See `example-plan.yml` and
`food-library.yml`. Running the script will add up daily macro totals
and print the difference between the target and the daily total.

``` sh
# Build the project.
stack build

# Run
stack exec nutrition <path-to-diet-plan>
```

[nutrition]: https://github.com/goosetherumfoodle/macro-calc
