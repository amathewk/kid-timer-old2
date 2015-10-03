package com.example.pingpong

import android.app.Activity
import android.os.Bundle
import android.speech.tts.TextToSpeech
import android.speech.tts.TextToSpeech.OnInitListener
import android.util.Log
import android.widget.{EditText, Button, TextView, LinearLayout}
import com.example.pingpong
import macroid._
import macroid.contrib._
import macroid.FullDsl._
import scala.concurrent.ExecutionContext.Implicits.global

import scala.collection.mutable
import scala.concurrent.Future

object OurTweaks {
  def textBox(defaultText: String)(implicit appCtx: AppContext) =
    TextTweaks.large +
      text(defaultText)

  def orient(implicit appCtx: AppContext) =
    landscape ? horizontal | vertical
}

class MainActivity extends Activity with Contexts[Activity] {
  val LOG_TAG = "MainActivity"
  var count = slot[TextView]
  var time = slot[TextView]
  var display = slot[TextView]

  var tts : TextToSpeech = null
  //  val startButton, stopButton

  import com.example.pingpong.CountingState._
  @volatile var countState : CountingState = Initializing

  override def onCreate(savedInstanceState: Bundle) = {
    super.onCreate(savedInstanceState)

    val view = l[LinearLayout](
      w[Button] <~
        text("Countdown!") <~
        On.click {
          startCount()
          display <~ text(s"${count.get.getText}, ${time.get.getText}")
        },
      w[EditText] <~
        wire(count) <~
        OurTweaks.textBox("10"),
      w[EditText] <~
        wire(time) <~
        OurTweaks.textBox("60"),
      w[TextView] <~
        wire(display)
    ) <~ OurTweaks.orient

    setContentView(getUi(view))
    tts = new TextToSpeech(getApplicationContext(), ttsInitListener)
  }

  def startCount() = {
    val countVal : String =  count.get.getText.toString
    val timeVal : String =  time.get.getText.toString
    val convertResult: ConvertResult = convert(countVal, timeVal)
    if(convertResult.valid == true) {
      Future {
        countdown(convertResult.count.get, convertResult.time.get)
      }
    } else {
      val msg = convertResult.msgs.foldLeft("") { (s1: String, s2: String) => s1 + "\n" + s2 }
//      display <~ text(msg)
    }
  }

  /** validates the count and integer entries */
  def convert(count: String, time: String) : ConvertResult = {
    var valid = true
    val msgs = mutable.HashSet[String]()
    if( ! (count forall Character.isDigit) || ! (time forall Character.isDigit) ) {
      valid = false
      msgs += "Count and time must be numbers"
      return ConvertResult(valid, msgs, None, None)
    }
    val countVal : Int = count.toInt
    if(countVal > 1000) {
      valid = false
      msgs += "count cannot be greater than 1000"
    }
    val timeVal : Int = time.toInt
    if(timeVal > 1000) {
      valid = false
      msgs += "time cannot be greater than 1000"
    }
    if(valid) {
      val delay:Float = (timeVal*1000)/countVal
      if(delay < 50) {
        valid = false
        msgs += "The count is too high for the specified time. Either reduce the count or increase the time."
        return ConvertResult(valid, msgs, None, None)
      }
    }
    ConvertResult(valid, msgs, Some(countVal), Some(timeVal))
  }

  def ttsInitListener = new OnInitListener {
    override def onInit(i: Int): Unit = {
      countState = Ready
    }
  }

  def countdown(count: Int, time: Int) : Unit = {
    Log.i(LOG_TAG, s"countdown starting... count = $count, time = $time")
    Log.d(LOG_TAG, s"countState = $countState")
    if(countState == Counting) return
    while(countState == Initializing) {}
    countState = Counting

    val startTime = System.nanoTime()
    val delayInMillis = (time * 1000 )/ count
    Log.i(LOG_TAG, s"Delay $delayInMillis")

    for(i <- 1 to count){
      tts.speak(i.toString, TextToSpeech.QUEUE_ADD, null)
      if(countState == Interrupting) {
        countState = Ready
        return
      }
      Thread.sleep(delayInMillis)
    }
    println((System.nanoTime() - startTime))
    countState = Ready
    //    toast(count)
  }

}

case class ConvertResult(valid: Boolean, msgs: mutable.Set[String], count: Option[Int], time: Option[Int])

object CountingState extends Enumeration {
  type CountingState = Value
  val Initializing, Ready, Counting, Interrupting = Value
}