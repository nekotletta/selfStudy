# create the graph
# traverse in bfs and dfs
import heapq
class Graph:
    def __init__(self, edges=0):
        self.edges = edges
        self.graph = []
        self.adj_list = []
        for index in range(self.edges):
            self.graph.append([0]*self.edges)
        for index in range(self.edges):
            self.adj_list.append([])

    def add_edge(self, source, dest, weight=0):
        self.graph[source][dest] = weight
        self.graph[dest][source] = weight

        self.adj_list[source].append([dest, weight])
        # self.adj_list[dest].append([source, weight])

    

    def display_graph(self):
        total_nodes = len(self.graph)
        print("Matrix repr:\n")
        for i in range(total_nodes):
            for j in range(total_nodes):
                print(self.graph[i][j], end=" ")
            print()
        print()
        print("Adjacency repr:\n")
        for i in range(len(self.adj_list)):
            print(f"{i} --> {self.adj_list[i]}")

    # go as deep as u can
    def traverse(self, adj_l, val, visited, traversal):
        visited[val] = True
        traversal.append(val)

        for list_val in adj_l[val]:
            if not visited[list_val[0]]:
                self.traverse(adj_l, list_val[0], visited, traversal)

    def dfs(self):
        visited = [False] * len(self.adj_list)
        traversal = []

        for index in range(len(self.adj_list)):
            if not visited[index]:
                self.traverse(self.adj_list, index, visited, traversal)
            
        return traversal

    def dijkstra(self, source):
        if not self.adj_list[source]:
            return "No neighbors to visit"

        queue = []
        visited = set()
        distances = [float('inf')] * len(self.adj_list)
        distances[source] = 0
        heapq.heappush(queue, (source, 0))

        while queue:
            curr_node, curr_dist = heapq.heappop(queue)
            if curr_dist > distances[curr_node] or curr_node in visited: continue
            visited.add(curr_node)
            for node, weight in self.adj_list[curr_node]:
                if node not in visited:
                    distances[node] = min(distances[curr_node] + weight, distances[node])
                    heapq.heappush(queue, (node, distances[node]))

        return distances

    def universal_traversal(self, source=0):
        # visited is the visited set
        # stack is whats left
        stack = [(0, source)]
        # stack = []
        # heapq.heappush(stack, (0, source))
        visited = set()
        traversal = []

        # i want the top to process
        while stack:
            # curr_cost, curr_source = stack.pop(0)
            curr_cost, curr_source = heapq.heappop(stack)
            if curr_source not in visited:
                for node, cost in self.adj_list[curr_source]:
                    # stack.append((cost + curr_cost, node))

                    heapq.heappush(stack, (curr_cost + cost, node))
                visited.add(curr_source)
                traversal.append(curr_source)

        return traversal


g = Graph(5)
g.add_edge(0, 1, 1)
g.add_edge(3, 0, 4)
g.add_edge(2, 3, 1)
g.add_edge(0, 2, 2)
# g.add_edge(2, 1, 1)
g.add_edge(0, 4, 2)
# g.add_edge(0, 3, 4)
g.add_edge(1, 2, 1)

g.display_graph()
print(g.dfs())
print("Visiting:")
print(g.dijkstra(0))
print(g.dijkstra(3))
print(g.dijkstra(4))
print(g.universal_traversal())