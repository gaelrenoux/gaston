# Gaston

Gaston is a tool to build schedules for conventions and such. It was build for RPG conventions.

It works by generating schedules and adjusting them, trying to get the best possible global score. The global score is a
function giving more importance to people with a low score (ensuring the end schedule is fair, not sacrificing
completely a person's satisfaction so that other can have a great schedule).



## Running Gaston

Clone the repository, and build with: `sbt assembly`

This produces a fat jar in target/scala-2.12, which you can then run with:
```
java -jar target/scala-2.12/gaston-assembly-0.2-SNAPSHOT.jar --from problem.conf
```

To get the full list of command line options, use:
```
java -jar target/scala-2.12/gaston-assembly-0.2-SNAPSHOT.jar --help
```



## Input file

The input file describes the problem to solve. It follows the HOCON format. To have an idea of how the score values
measure, each person has 1000 points distributed over their wishes.

- `gaston`: Root element of the configuration.
    - `settings`: Global settings. All fields are optional and have a default value.
        - `incompatibility-anti-preference`: Negative score for a person forced to share a topic with someone it would rather not. Default value is `-1000`.
        - `default-max-topics-per-slot`: Max number of topics per slot (default value is no limit.
        - `default-min-persons-per-topic`: Minimum number of persons per topic, can be overriden on specific topics. Default value is `1`.
        - `default-max-persons-per-topic`: Maximum number of persons per topic, can be overriden on specific topics. Default value is `10`.
        - `max-persons-on-nothing`: On each slot, maximum number of persons who can have no topic scheduled. Default value is `0`, i.e. everyone must have a topic scheduled on each slot.
        - `min-persons-on-nothing`: On each slot, minimum number of persons who can have no topic scheduled (the idea being that enough unoccupied persons can do something together). Default value is `0`, i.e. no minimum.
        - `person-on-nothing-anti-preference`: Negative score attached to a person doing nothing on a slot. Default value is `-100`.
        - `backtrack-initial-schedule`: Technical stuff: should initial schedules be empty or backtracked ? Best answer depends on the schedule being highly constrained or note. Default value is `true`.
    - `tableSettings`: Settings for importing a table. Optional (leave that field out if you are not importing a table). See the *Importing a table* section below for details. 
    - `slots`: The slots on which to schedule, as an array of arrays of slots. The inner arrays contain the slots following each other directly (such as one in the morning and the other in the afternoon of the same day), so that mult-slot topics may be handled (WIP). The following lines describe the structure of one slot.
        - `name`: Name of the slot. Must be unique among slots.
        - `max-topics`: Maximum number of topics to schedule on this slot (e.g. because space as limited at that time).
    - `topics`: The topics to schedule. The following lines describe the structure of one topic.
        - `name`: Name of the topic. Must be unique among topics.
        - `min`: Minimum number of persons on a topic. Default value is empty (will use the settings' value).
        - `max`: Maximum number of persons on a topic. Default value is empty (will use the settings' value).
        - `occurrences`: How many sessions of the topic are possible. Default value is 1.
        - `multiple`: How many parallel sessions this topic represents (all sessions are simultaneous). Note than min and max are for one session. Default value is 1.
        - `slots`: An array of slot names on which the topic can be scheduled. Default value is empty (can be scheduled on any slot).  
    - `persons`: The persons to schedule. The following lines describe the structure of one person.
        - `name`: Name of the person. Must be unique among persons.
        - `weight`: A person with a higher weight counts more when calculating the global score. Specifically, that person's individual score will be divided by its weight, bringing them down and making the schedule optimizing more for them.. Default value is `1`.
        - `absences`: An array of slot names on which this person is missing, and therefore cannot be scheduled. Default value is an empty array.
        - `mandatory`: An array of topic names for which this person is mandatory: it must be present on those topics. Default value is an empty array.
        - `forbidden`: An array of topic names for which this person is forbidden: it cannot be scheduled on those topics. Default value is an empty array.
        - `incompatible`: An array of person names this person wish not to share a topic with. Default value is an empty array.
        - `wishes`: A mapping of topic names to scores. The higher the score, the more that persons wishes to be on that topic.
    - `constraints`: Global constraints on the schedule.
        - `simultaneous`: An array of simultaneity constraints.
            - `topics`: An array of topics. Those topics must all be scheduled on the same slot, or not at all.
        - `exclusive`: An array of exclusivity constraints.
            - `topics`: An array of topics. A person can only be scheduled on one of these topics.

You can generate an input file with old default values with:
```
java -jar target/scala-2.12/gaston-assembly-0.2-SNAPSHOT.jar --generate-input
```


### Example

```hocon
gaston = {

  settings.default-max-topics-per-slot = 3

  slots = [
    [
      {
        "name": "D1-afternoon",
        "max-topics": 4
      }, {
        "name": "D1-evening"
      }
    ], [
      {
        "name": "D2-afternoon",
        "max-topics": 4
      }, {
        "name": "D2-evening"
      }
    ]
  ]

  persons = [
    {
      absences = ["D1-afternoon"]
      name = "Anatole"
      weight = 1.0
      mandatory = ["Gamma"]
      forbidden = ["Alpha"]
      wishes = {
        "Beta" = 10
        "Gamma" = 10
        "Delta" = 10
      }
    },
    {
      incompatible = ["Anatole", "Colin"]
      name = "Bernadette"
      weight = 1.0
      mandatory = ["Alpha"]
      wishes = {
        "Delta" = 10
      }
    },
    {
      absences = ["D1-afternoon", "D1-evening"]
      name = "Colin"
      weight = 1.5
      mandatory = ["Beta"]
      wishes = {
        "Alpha" = 1
        "Delta" = 10
      }
    },
    {
      name = "Dominique"
      weight = 1.5
      mandatory = ["Beta"]
      wishes = {
        "Alpha" = 1
        "Delta" = 10
      }
    },
    {
      name = "Eric"
    },
    {
      name = "Françoise"
    },
    {
      name = "Garfield"
    },
    {
      name = "Harley"
    }
  ]

  topics = [
    {
      max = 5
      name = "Alpha"
    },
    {
      max = 4
      min = 4
      name = "Beta"
    },
    {
      name = "Gamma"
    },
    {
      min = 5
      name = "Delta"
    },
    {
      max = 6
      min = 3
      name = "Epsilon"
    },
    {
      max = 6
      min = 3
      name = "Zeta"
    },
    {
      max = 6
      min = 3
      name = "Eta"
    }
  ]

  constraints = {
    simultaneous = [
      {topics = ["Eta", "Zeta"]}
    ]
    exclusive = [
      {topics = ["Eta", "Zeta"]},
      {topics = ["Beta", "Gamma"], exemptions = ["Eric"]}
    ]
  }
}
```



## Importing a table

Quite often, the wishes of the participant will be expressed in a double-entry table. The importing feature can transform this table into a HOCON file.

The table must have the topics in lines, and each person in a column (this may become selectable in the future). To import a table:
```
java -jar target/scala-2.12/gaston-assembly-0.2-SNAPSHOT.jar --from params.conf --from-table table.txt --generate-input
```

The configuration file will hold the information necessary to properly import the table, as well as basic information not present in the tables (settings and slots).

Note that you can use directly the table to generate schedules (by omitting the `--generate-input` option) but this is not recommended. You should generate the input, check it (and possibly modifiy it), then generate schedules using the input.

The parameters for the import are in the `tableSettings` section of the input file. All indexes start at 0 (first line is index 0, second line is index 1, and so on).
- `separator`: Separator between columns on a single line (should be inside quotation marks). Default value is `\t` (tabulation). 
- `persons-row`: Row for the persons' names. Default value is `0` (first row).
- `wishes-start-row`: First row of wishes, typically one because there's the persons' line before. Default value is `1` (wishes start on the second line).
- `persons-start-col`: First column containing a person. Default value is `4`.
- `topic-col`: Column for the topics' names. Default value is `0`. 
- `topic-occurrences-col`: Column for the topics' occurrences (see field `gaston.topics.occurrences`). Default value is empty, all occurrences being let at their default value of `1`. 
- `mandatory-person-col`: Column for a mandatory person on a topic (see field `gaston.persons.mandatory`, there is often only one person per topic). It is not possible to set multiple mandatory persons on a topic in the table, this is something you will have to do by modifying the input file afterwards. Default value is `1`. 
- `min-persons-col`: Column for the min number of persons on a topic (see field `gaston.topics.min`). Default value is empty (topics won't have a specific min). 
- `max-persons-col`: Column for the max number of persons on a topic (see field `gaston.topics.max`). Default value is empty (topics won't have a specific max). 
- `persons-count-add`: A number to add to the min and max number of persons read, because sometimes the mandatory person isn't counted in the table. Default value is `0`. 
- `mandatory-person-weight`: A special weight for a person if it is mandatory on at least one topic (see field `gaston.persons.weight`). Default value is `1`, i.e. same as everyone else. 
- `forbidden-person-marker`: This value in a wish cell indicates that that person is forbidden on that topic (see field `gaston.persons.forbidden`). Default value is `0`.
- `preferences-score-mapping`: An object mapping for each possible text in a wish cell, the associated score. Unknown texts are ignored. Optional, if the field is missing (or set to `null`), the text in the cell will be converted as the number and used (if the text is not a number, it is ignored).

### Example input

```hocon
gaston {
  
  settings {
      default-max-topics-per-slot = 3
      minPersonsOnNothing = 3
      maxPersonsOnNothing = 10
  }
  
  table-settings {
      separator = "\t"
      persons-row: 0
      wishes-start-row = 1
      persons-start-col = 4
      topic-col = 0
      topic-occurrence-count-col = null
      mandatory-person-col = 1
      min-persons-col = 2
      max-persons-col = 3
      persons-count-add = 0
      mandatory-person-weight = 1.5
      forbidden-person-marker = 0
      preferences-score-mapping = {
        "+" = 1.0
        "++" = 2.0
        "+++" = 5.0
      }
  }
  
  slots = [
    [
      {
        "name": "D1-afternoon",
        "max-topics": 4
      }, {
        "name": "D1-evening"
      }
    ], [
      {
        "name": "D2-afternoon",
        "max-topics": 4
      }, {
        "name": "D2-evening"
      }
    ]
  ]
}
```
