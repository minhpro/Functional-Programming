public class Room implements Comparable {
    public int label;
    public int value;
    public Room east;
    public Room west;
    public Room south;
    public Room north;
    public List<Room> neighbors;

    public int color; //0-White, 1-Gray, 2-Black (int breath first search)
    public Room predecessor;
    public int d;

    public Room(int label, int value, Room east, Room west, Room south, Room north) {
        this.label = label;
        this.value = value;
        this.east = east;
        this.west = west;
        this.south = south;
        this.north = north;
        neighbors = new ArrayList<>();
        if (east != null)
            neighbors.add(east);
        if (west != null)
            neighbors.add(west);
        if (south != null)
            neighbors.add(south);
        if (north != null)
            neighbors.add(north);
    }

    @Override
    public int compareTo(Object o) {
        if (o instanceof Room) {
            Room other = (Room) o;
            return d - other.d;
        } else
            return 0;
    }



public void breathFirstSearch(Room s, Room f) {
        s.d = MAX_POINT;
        PriorityQueue<Room> q = new PriorityQueue<>();

        for (Room r : roomList) {
            if (r.label != s.label) {
                r.color = 0;
                r.d = Integer.MAX_VALUE;
                r.predecessor = null;
            }
            q.offer(r);
        }

        while (!q.isEmpty()) {
            Room u = q.poll();
            if (u.label == f.label)
                return;
            for (Room v : u.neighbors) {
                int alt = u.d + 1 - v.value;
                if (alt < v.d) {
                    v.d = alt;
                    v.predecessor = u;
                    q.remove(v);
                    q.offer(v);
                }
            }
        }
    }

    public Stack<Room> constructPath(Room f) {
        Stack<Room> roomList = new Stack<>();
        roomList.push(f);
        while (f.predecessor != null) {
            roomList.push(f.predecessor);
        }

        return roomList;
    }