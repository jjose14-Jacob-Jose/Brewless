package interpreter;

import org.jpl7.*;

public class PrologExample {
    public static void main(String[] args) {
//        Query query = new Query("consult", new Term[] {new Atom("sample.pl")});
        Query query =
                new Query(
                        "consult",
                        new Term[] {new Atom("test.pl")}
                );

        boolean result = query.hasSolution();
        if (result) {
            Query q = new Query("father", new Term[] {new Atom("alice"), new Atom("X")});
            if (q.hasSolution()) {
                System.out.println("Solution found!");
            } else {
                System.out.println("No solution found.");
            }
        } else {
            System.out.println("Failed to load Prolog file.");
        }
    }
}
