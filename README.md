# Gaston

Gaston is a tool to build schedules for conventions and such. It was build for RPG conventions.

It works by generating schedules and adjusting them, trying to get the best possible global score. The global score is a
function giving more importance to people with a low score (ensuring the end schedule is fair, not sacrificing
completely a person's satisfaction so that other can have a great schedule).



## Running Gaston

Clone the repository, and build with: `sbt assembly`

This produces a fat jar in target/scala-2.12, which you can then run with:

`java -jar target/scala-2.12/gaston-assembly-0.2-SNAPSHOT.jar --from problem.conf`



## Input file

The input file describes the problem to solve. It follows the HOCON format. To have an idea of how the score values
measure, each person has 1000 points distributed over their wishes.

- `gaston`: Root element of the configuration.
    - `settings`: Global settings. All fields are optional and have a default value.
        - `incompatibility-anti-preference`: Negative score for a person forced to share a topic with someone it would rather not. Default value is `-1000`.
        - `default-max-topics-per-slot`: Max number of topics per slot (default value is no limit.
        - `default-min-persons-per-topic`: Minimum number of persons per topic, can be overriden on specific topics. Default value is `1`.
        - `default-max-persons-per-topic`: Maximum number of persons per topic, can be overriden on specific topics. Default value is `10`.
        - `max-persons-on-nothing`: On each slot, maximum number of persons who can have no topic scheduled. Default value is `0`, i.e. everyone must have a topic scheduled on each slot.
        - `min-persons-on-nothing`: On each slot, minimum number of persons who can have no topic scheduled (the idea being that enough unoccupied persons can do something together). Default value is `0`, i.e. no minimum.
        - `person-on-nothing-anti-preference`: Negative score attached to a person doing nothing on a slot. Default value is `-100`.
        - `backtrack-initial-schedule`: Technical stuff: should initial schedules be empty or backtracked ? Best answer depends on the schedule being highly constrained or note. Default value is `true`.
    - `tableSettings`: Settings for importing a table. Optional (leave that field out if you are not importing a table). Remember that all indexes start at 0 (first line is index 0, second line is index 1, and so on). See the *Importing a table* section below for details. 
        - `separator`: Separator between fields (should be inside quotation marks). Default value is `\t` (tabulation). 
        - `persons-row`: Row where the person names appear. Default value is `0` (first row).
        - `wishes-start-row`: First row of wishes, typically one because there's the persons' line before. Default value is `1` (wishes start on the second line).
        - `persons-start-index`: First column containing a person on the persons' line. Default value is `4`.
        - `topic-index`: The topics's column. Default value is `0`. 
        - `topic-occurrence-count-index`: The column indicating how many sessions of the topic are possible. Default value is empty, meaning all topics can only happen once. 
        - `mandatory-person-index`: The column indicating the mandatory person on a topic. Default value is `1`. 
        - `min-persons-index`: The column indicating the min number of persons on a topic. Default value is empty (will use the settings' value). 
        - `max-persons-index`: The column indicating the max number of persons on a topic. Default value is empty (will use the settings' value). 
        - `persons-count-add`: A number to add to the min and max number of persons read, because sometimes the mandatory person isn't counted in the table. Default value is `0`. 
        - `mandatory-person-weight`: A special weight for a person if it is mandatory on at least one topic (see `persons.weight` below). Default value is `1`, i.e. same as everyone else.  
        - `forbidden-person-marker`: This value in a wish cell indicates that that person is forbidden on that topic. Default value is `0`.
        - `preferences-score-mapping`: For a text in a wish cell, the associated score. Unknown texts are ignored. Optional, if the field is missing (or set to `null`), the text in the cell will be converted as the number and used (if the text is not a number, it is ignored).
    - `slots`: The slots on which to schedule, as an array of arrays of slots. The inner arrays contain the slots following each other directly (such as one in the morning and the other in the afternoon of the same day), so that mult-slot topics may be handled (WIP). The following lines describe the structure of one slot.
        - `name`: Name of the slot. Must be unique among slots.
        - `max-topics`: Maximum number of topics to schedule on this slot (e.g. because space as limited at that time).
    - `topics`: The topics to schedule. The following lines describe the structure of one topic.
        - `name`: Name of the topic. Must be unique among topics.
        - `min`: Minimum number of persons on a topic. Default value is empty (will use the settings' value).  
        - `max`: Maximum number of persons on a topic. Default value is empty (will use the settings' value).  
        - `occurences`: How many sessions of the topic are possible. Default value is 1.
        - `slots`: An array of slot names on which the topic can be scheduled. Default value is empty (can be scheduled on any slot).  
    - `persons`: The persons to schedule. The following lines describe the structure of one person.
        - `name`: Name of the person. Must be unique among persons.
        - `weight`: A person with a higher weight counts more when calculating the global score. Specifically, that person's individual score will be divided by its weight, bringing them down and making the schedule optimizing more for them.. Default value is `1`.
        - `absences`: An array of slot names on which this person is missing, and therefore cannot be scheduled. Default value is an empty array.
        - `mandatory`: An array of topic names for which this person is mandatory: it must be present on those topics. Default value is an empty array.
        - `forbidden`: An array of topic names for which this person is forbidden: it cannot be scheduled on those topics. Default value is an empty array.
        - `incompatible`: An array of person names this person wish not to share a topic with. Default value is an empty array.
        - `wishes`: A mapping of topic names to scores. The higher the score, the more that persons wishes to be on that topic.
    - `constraints`: Global constraints on the schedule.
        - `simultaneous`: An array of simultaneity constraints.
            - `topics`: An array of topics. Those topics must all be scheduled on the same slot, or not at all.
        - `exclusive`: An array of exclusivity constraints.
            - `topics`: An array of topics. A person can only be scheduled on one of these topics.



## Importing a table
