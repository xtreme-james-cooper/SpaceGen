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

import java.awt.Graphics2D
import java.awt.image.BufferStrategy
import javax.swing.JOptionPane

class GameThread(input : Input, display : GameDisplay, controls : GameControls, bs : BufferStrategy) {

  def run(tickFun : Unit => Boolean) : Unit =
    try {
      innerRun(tickFun)
    } catch {
      case e : Exception => {
        e.printStackTrace
        JOptionPane.showMessageDialog(null, e.getMessage)
      }
    }

  private def innerRun(tickFun : Unit => Boolean) : Unit = {
    controls.processInput
    val done : Boolean = tickFun()
    display.draw(bs.getDrawGraphics.asInstanceOf[Graphics2D], input.mouse)
    bs.show
    Thread.sleep(25)
    if (!done)
      innerRun(tickFun)
  }

}
