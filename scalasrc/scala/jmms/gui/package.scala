package jmms

import java.awt.event.{ActionEvent, ActionListener}

/**
  * Created by weijiayi on 19/10/2016.
  */
package object gui {
  type CallBack = () => Unit

  def callback(action: =>Unit) = () => action

  def mkAction(action: =>Unit) = new ActionListener {
    override def actionPerformed(e: ActionEvent): Unit = action
  }
}
