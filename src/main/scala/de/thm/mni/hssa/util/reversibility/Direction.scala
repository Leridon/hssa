package de.thm.mni.hssa.util.reversibility

enum Direction {
  case FORWARDS
  case BACKWARDS

  def inverse = this match
    case FORWARDS => BACKWARDS
    case BACKWARDS => FORWARDS
}