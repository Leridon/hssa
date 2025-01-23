package de.thm.mni.hybridcomputing.util

object FormattingUtilities {
    def parenthesize(inner: String, count: Int = 1, open: Char = '(', close: Char = ')'): String = {
        if (count <= 0) inner
        else open + parenthesize(inner, count - 1, open, close) + close
    }
    
    def count_parens(s: String): Int = {
        var open_count = 0
        var last_open_count = 0
        
        s.foreach(c => {
            if (c == '(') open_count += 1
            else if (c == ')') open_count -= 1
            else last_open_count = open_count
        })
        
        last_open_count
    }
}
