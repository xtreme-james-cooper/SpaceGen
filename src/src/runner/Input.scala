/**
 * Copyright 2012 David Stark
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package src.runner

import java.awt.event.{ MouseMotionListener, MouseListener, MouseEvent, KeyListener, KeyEvent }
import java.awt.Point

class Input extends KeyListener with MouseListener with MouseMotionListener {

  val keys : Array[Boolean] = Array.ofDim(65536)
  var mouse : Option[Point] = None

  def keyDown(code : Int) : Boolean = keys(code)

  override def keyPressed(ke : KeyEvent) : Unit = keys(ke.getKeyCode) = true

  override def keyReleased(ke : KeyEvent) : Unit = keys(ke.getKeyCode) = false

  override def mouseEntered(me : MouseEvent) : Unit = mouse = Some(me.getPoint)

  override def mouseExited(me : MouseEvent) : Unit = mouse = None

  override def mouseMoved(me : MouseEvent) : Unit = mouse = Some(me.getPoint)

  override def mouseClicked(me : MouseEvent) : Unit = ()

  override def mousePressed(me : MouseEvent) : Unit = ()

  override def mouseReleased(me : MouseEvent) : Unit = ()

  override def keyTyped(ke : KeyEvent) : Unit = ()

  override def mouseDragged(me : MouseEvent) : Unit = ()

}
