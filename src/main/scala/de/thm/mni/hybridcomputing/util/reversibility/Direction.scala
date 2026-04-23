package de.thm.mni.hybridcomputing.util.reversibility

enum Direction {
    case FORWARDS
    case BACKWARDS
    
    def inverse: Direction = this match
        case FORWARDS => BACKWARDS
        case BACKWARDS => FORWARDS
    
    def choose[T](fw: => T, bw: => T): T = this match
        case FORWARDS => fw
        case BACKWARDS => bw
}