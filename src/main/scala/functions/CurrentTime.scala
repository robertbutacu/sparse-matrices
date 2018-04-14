package functions

import java.util.Calendar

object CurrentTime {
  def printCurrentTime(): String = {
    val now = Calendar.getInstance()
    val currentHour = now.get(Calendar.HOUR)
    val currentMinute = now.get(Calendar.MINUTE)
    val currentSecond = now.get(Calendar.SECOND)
    val currentMilli = now.get(Calendar.MILLISECOND)

    s"""$currentHour : $currentMinute : $currentSecond :: $currentMilli"""
  }
}
