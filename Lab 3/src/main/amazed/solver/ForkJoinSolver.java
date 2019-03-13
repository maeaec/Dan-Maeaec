package amazed.solver;

import amazed.maze.Maze;

import java.util.List;
import java.util.LinkedList;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.concurrent.ConcurrentSkipListSet;


/**
 * <code>ForkJoinSolver</code> implements a solver for
 * <code>Maze</code> objects using a fork/join multi-thread
 * depth-first search.
 * <p>
 * Instances of <code>ForkJoinSolver</code> should be run by a
 * <code>ForkJoinPool</code> object.
 */


public class ForkJoinSolver extends SequentialSolver {

    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal.
     *
     * @param maze the maze to be searched
     */

    // using a thread safe data structure.
    public ForkJoinSolver(Maze maze) {
        super(maze);
        visited = new ConcurrentSkipListSet<>();
    }

    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal, forking after a given number of visited
     * nodes.
     *
     * @param maze      the maze to be searched
     * @param forkAfter the number of steps (visited nodes) after
     *                  which a parallel task is forked; if
     *                  <code>forkAfter &lt;= 0</code> the solver never
     *                  forks new tasks
     */
    public ForkJoinSolver(Maze maze, int forkAfter) {
        this(maze);
        this.forkAfter = forkAfter;
    }

    // a constructor making sure that visited is shared between processes
    public ForkJoinSolver(Maze maze, Set<Integer> visited) {
        this(maze);
        this.visited = visited;
    }

    /**
     * Searches for and returns the path, as a list of node
     * identifiers, that goes from the start node to a goal node in
     * the maze. If such a path cannot be found (because there are no
     * goals, or all goals are unreacheable), the method returns
     * <code>null</code>.
     *
     * @return the list of node identifiers from the start node to a
     * goal node in the maze; <code>null</code> if such a path cannot
     * be found.
     */


    @Override
    public List<Integer> compute() {
        return parallelSearch();
    }

    private List<Integer> parallelSearch() {
        int player = maze.newPlayer(start);
        // start with start node
        frontier.push(start);
        // as long as not all nodes have been processed
        while (!frontier.empty()) {
            // get the new node to process
            int current = frontier.pop();
            // if current node has a goal
            if (maze.hasGoal(current)) {
                // move player to goal
                maze.move(player, current);
                // search finished: reconstruct and return path
                return pathFromTo(start, current);
            }
            // if current node has not been visited yet, add it to visited
            if (visited.add(current)) {
                // move player to current node
                maze.move(player, current);
                // for every node nb adjacent to current
                for (int nb : maze.neighbors(current)) {
                    // if nb has not been already visited,
                    if (!visited.contains(nb)) {
                        // add nb to the nodes to be processed
                        frontier.push(nb);
                        // nb can be reached from current (i.e., current is nb's predecessor)
                        predecessor.put(nb, current);
                    }
                }
                // If there are multiple paths to go
                if (frontier.size() > 1) {
                    List<ForkJoinSolver> children = new ArrayList();
                    while (frontier.size() > 0) {
                        // creates a new forkjoinsolver process for each child, folowing one path each
                        ForkJoinSolver child = new ForkJoinSolver(maze, visited);
                        child.start = frontier.pop();
                        children.add(child);
                        child.fork();
                    }
                    List<Integer> childPath = new LinkedList<>();
                    for (int i = 0; i < children.size(); i++) {
                        childPath = children.get(i).join();
                        if(childPath != null) {
                            // if a child returns a path to the goal, return recursively.
                            List<Integer> parentPath = pathFromTo(start, current);
                            List<Integer> totalPath = new LinkedList<>();
                            totalPath.addAll(parentPath);
                            totalPath.addAll(childPath);
                            return totalPath;
                        }
                    }
                    // dead end
                    return null;
                }
                // When frontier is smaller than 2 it will be taken care of by the while-loop
            }
        }
        // all nodes explored, no goal found
        return null;
    }
}