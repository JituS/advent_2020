from typing import NamedTuple

with open('01.txt', 'r') as f:
    data = [(x.strip()[:1], int(x.strip()[1:])) for x in f.readlines()]


class Point(NamedTuple):
    x: int
    y: int


NORTH = Point(0, 1)
EAST = Point(1, 0)
SOUTH = Point(0, -1)
WEST = Point(-1, 0)


class Boat:
    x = 0
    y = 0
    directions = (NORTH, EAST, SOUTH, WEST)
    facing = EAST
    dir_index = {
        'N': NORTH,
        'E': EAST,
        'W': WEST,
        'S': SOUTH
    }

    def move(self, direction, distance):
        for x in range(distance):
            self.x += direction[0]
            self.y += direction[1]

    def forward(self, distance):
        self.move(self.facing, distance)

    def move_direction(self, direction, distance):
        self.move(self.dir_index[direction], distance)

    def right(self, degrees):
        for rotations in range(degrees // 90):
            self.facing = (self.directions + self.directions)[self.directions.index(self.facing) + 1]

    def left(self, degrees):
        for rotations in range(degrees // 90):
            self.facing = self.directions[self.directions.index(self.facing) - 1]

    def to_waypoint(self, waypoint, distance):
        for _ in range(distance):
            self.x += waypoint.x
            self.y += waypoint.y


class Waypoint(Boat):
    x = 10
    y = 1

    def right(self, degrees):
        for rotations in range(degrees // 90):
            self.x, self.y = self.y, -self.x

    def left(self, degrees):
        for rotations in range(degrees // 90):
            self.x, self.y = -self.y, self.x


boat = Boat()
for direction, distance in data:
    if direction in 'NEWS':
        boat.move_direction(direction, distance)
    elif direction == 'F':
        boat.forward(distance)
    elif direction == 'R':
        boat.right(distance)
    elif direction == 'L':
        boat.left(distance)
print('Part 1:', abs(boat.x) + abs(boat.y))

boat = Boat()
waypoint = Waypoint()
for direction, distance in data:
    if direction in 'NEWS':
        waypoint.move_direction(direction, distance)
    elif direction == 'F':
        boat.to_waypoint(waypoint, distance)
    elif direction == 'R':
        waypoint.right(distance)
    elif direction == 'L':
        waypoint.left(distance)
    print(distance, boat.x, boat.y, waypoint.x, waypoint.y)
print('Part 2:', abs(boat.x) + abs(boat.y))
