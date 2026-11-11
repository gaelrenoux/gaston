- Make a big pass on naming:
  - Rename unassigned stuff to holes (someone is not assigned on a slot). Rename the unassigned topic to maybe "free-time" or something like that.
  - Make slot-cycle a first class citizen, not just an idea (the slots are just a list of lists, there's no slot-cycle class).

- Add unit test for forced topics
- When a schedule is a success (better than previous one), try to re-assign everyone a few times to see if you can improve it further
- Better handling of constraints
- Incompatibility reward should be in the preference (in the config), not in settings
- Create a max-unassigned or max-nothing, and use a topic-exclusive ? It's not a personal pref though, so it doesn't work that well
- Keep and display schedules that are close to the current one, to give some alternatives (or fix a target score and give all that are above the score)
- Simulated cooling
- Generate a better base configuration, depending on what the user asks for
- Exclusive constraint on the topic level (not just persons are exclusive, it's only one topic is present at all)


- When reading the table format: check that a mandatory person isn't also put as forbidden on the same topic.
- Multi-slots topics break down when there can be more than two slots that follow each other.

# Reorganizing tests
Make a specific module for past events configuration re-testing
Then we can have a module for the benchmarks
Move regression testing (like special cases) into the main tests, they shouldn't be that slow
