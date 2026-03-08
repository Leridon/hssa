package de.thm.mni.hybridcomputing.util

object FormattingUtilities {
    def parenthesize(inner: String, count: Int = 1, open: Char = '(', close: Char = ')'): String = {
        if (count <= 0) inner
        else open + parenthesize(inner, count - 1, open, close) + close
    }
    
    def count_parens(s: String): Int = {
        @annotation.tailrec
        def loop(str: String, acc: Int): Int =
            if (isWrapped(str)) loop(str.substring(1, str.length - 1), acc + 1)
            else acc
        
        def isWrapped(str: String): Boolean = {
            if (str.length < 2 || str.head != '(' || str.last != ')') return false
            
            var depth = 0
            for (i <- 0 until str.length - 1) {
                if (str(i) == '(') depth += 1
                else if (str(i) == ')') depth -= 1
                
                if (depth == 0) return false
            }
            
            true
        }
        
        loop(s, 0)
    }
}
