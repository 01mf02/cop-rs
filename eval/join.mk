# Source:
# <https://stackoverflow.com/questions/9551416/gnu-make-how-to-join-list-and-separate-it-with-separator/9551487#9551487>

# A literal space.
space :=
space +=

# Joins elements of the given list with the given separator.
#   1. Element separator.
#   2. The list.
join-with = $(subst $(space),$1,$(strip $2))
