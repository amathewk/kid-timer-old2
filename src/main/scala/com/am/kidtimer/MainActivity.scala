package com.am.kidtimer

import android.app.Activity
import android.os.Bundle
import android.speech.tts.TextToSpeech
import android.speech.tts.TextToSpeech.OnInitListener
import android.util.Log
import android.view.Gravity
import android.widget._
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

  import com.am.kidtimer.CountingState._
  @volatile var countState : CountingState = Initializing

  val statusTextView = w[TextView]

  override def onCreate(savedInstanceState: Bundle) = {
    super.onCreate(savedInstanceState)
    val view = l[LinearLayout](
      w[Button] <~
        text("Countdown!") <~
        On.click {
          val convertResult : ConvertResult = startCount()
          val msgs = convertResult.msgs.foldLeft("") { (s1: String, s2: String) => s1 + "\n" + s2 }
          display <~ text(msgs)
        },
      w[EditText] <~
        wire(count) <~
        OurTweaks.textBox("5"),
      w[EditText] <~
        wire(time) <~
        OurTweaks.textBox("5"),
      statusTextView <~ wire(display)
    ) <~ OurTweaks.orient
//    statusTextView <~ gravity(Gravity.CENTER)

    setContentView(getUi(view))
    tts = new TextToSpeech(getApplicationContext(), ttsInitListener)
  }

   def startCount() : ConvertResult = {
    val countVal : String =  count.get.getText.toString
    val timeVal : String =  time.get.getText.toString
    val convertResult: ConvertResult = convert(countVal, timeVal)
    if(convertResult.valid == true) {
      val f = Future {
        countdown(convertResult.count.get, convertResult.time.get)
        "counting complete"
      }
//      runUi {
//        statusTextView <~ f.map(text)
//      }
      val msg = s"${count.get.getText}, ${time.get.getText}"
      Log.i(LOG_TAG, msg)
      statusTextView.get.setText(msg)
    } else {
      val msg = convertResult.msgs.foldLeft("") { (s1: String, s2: String) => s1 + "\n" + s2 }
      Log.i(LOG_TAG, s"convert result is not valid $msg")
    }
    return convertResult
  }

  def buttonPanel(implicit ctx: ActivityContext) =  l[FrameLayout](
  w[Button] <~
        text("Countdown!") <~
        On.click {
          val convertResult : ConvertResult = startCount()
          val msgs = convertResult.msgs.foldLeft("") { (s1: String, s2: String) => s1 + "\n" + s2 }
          display <~ text(msgs)
        })

  def formPanel(implicit ctx: ActivityContext)

  /** validates the count and integer entries and converts from String */
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
    if(countState == Counting) { Log.i(LOG_TAG, "Counting in progress... returning."); return }
    while(countState == Initializing) {}
    countState = Counting

    val startTime = System.nanoTime()
    val delayInMillis = (time * 1000 )/ count
    Log.i(LOG_TAG, s"Delay $delayInMillis")

    for(i <- 1 to count){
      Log.d(LOG_TAG, s"$i ...")
      tts.speak(i.toString, TextToSpeech.QUEUE_ADD, null)
      if(countState == Interrupting) {
        countState = Ready
        return
      }
      Thread.sleep(delayInMillis)
    }
    println((System.nanoTime() - startTime))
    countState = Ready
  }

}

case class ConvertResult(valid: Boolean, msgs: mutable.Set[String], count: Option[Int], time: Option[Int]) {
  def allMsgs = {msgs.foldLeft("") { (s1: String, s2: String) => s1 + "\n" + s2 }}
}

object CountingState extends Enumeration {
  type CountingState = Value
  val Initializing, Ready, Counting, Interrupting = Value
}