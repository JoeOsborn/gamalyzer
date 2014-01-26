* pick a trace
* repeat with this trace as "the current trace" until done (N times?):
  * compare the current trace to all other traces
    * use FTSE + edit distance or LCSS or smth for whole sequences
      * to compare a single input:
        * the left side of the input (before and including "selopt") is the determinant
        * the right side is a list ordered by significance
          * to compare two lists:
            * compare each element. each pair's diss. score is multiplied by the positional maximum, N-i / (N*(N+1)/2), where N is len.
            * to compare two lists: recurse
            * to compare two vecs:
              * compare each element. each pair's diss. score is multiplied by the positional maximum, 1 / N, where N is len.
            * to compare two numbers:
              * overapprox: if diff., diss. is 1; otherwise 0
              * better approx: interp between 0 (same) and 1 (diff.) based on epsilon-distance of A and B as a fraction of the witnessed range of this path.
                * witness the range of every path (e.g. [determinant,l(1),t(2),l(1)]) during parsing.
            * to compare two atoms: if diff. diss. is 1; otherwise 0
  * either:
    * find the trace least like any trace we've seen already (that we haven't already analyzed); that's the new current trace.
      * the V such that the smallest distance from each pivot P to V -- d(P,V) -- is as large as possible
    * or pick a random unseen trace; that's the new current trace.
* try to lay out based on this rectangular dissimilarity matrix