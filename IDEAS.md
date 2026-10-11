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




# Rework empty slots/zeros

Fix an expected number of empty-slots given your number of zeros.
- Can be linear: starting at 0 for no zero, and 2 for maybe 60% zeros (remember, you can have more empty-slots than planned, its just that they count for more, so that would mean that at 60% zeros it's already possible to get a third empty-slot)
- Or maybe use a logistic function (but not sure that's better than linear)

Then, when counting the negative costs of empty slot:
- Each full expected empty-slots count for the base amount (very low, e.g. -10, just to make sure it's still a drawback).
- The last, partiall full empty-slot count for the remaining ratio on empty-slots.

For example: with 1.3 expected empty-slots, the first empty-slot is the base cost (-10), then the second empty-slot is
the base cost plus 70% of the full cost (so - 10 - 140, if we put the full empty-slot at -200).

When having required empty-slots, you simply subtract back the negative cost for those empty-slot. So if our person at
1.3 had required 1 empty slot, they only get back +10. If they had required 2, they would get back +160.s