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

import java.awt.event.KeyEvent.{ VK_UP, VK_SPACE, VK_S, VK_RIGHT, VK_R, VK_LEFT, VK_DOWN }
import java.io.{ OutputStreamWriter, FileOutputStream, BufferedWriter }
import scala.math.{ min, max }
import javax.swing.{ JOptionPane, JFileChooser }

class GameControls(d : GameDisplay, w : GameWorld, input : Input) {

  def processInput : Unit = {
    if (input.keyDown(VK_UP)) w.stage.camY = max(w.stage.camY - 40, -500)
    if (input.keyDown(VK_DOWN)) w.stage.camY = min(w.stage.camY + 40, 2180)
    if (input.keyDown(VK_LEFT)) w.stage.camX = max(w.stage.camX - 40, -500)
    if (input.keyDown(VK_RIGHT)) w.stage.camX = min(w.stage.camX + 40, 2180)

    if (input.keyDown(VK_SPACE) && w.cooldown == 0) {
      w.stage.doTrack = true
      w.confirm = true
      w.cooldown = 8
    } else if (input.keyDown(VK_R) && w.cooldown == 0) {
      w.autorun = !w.autorun
      w.cooldown = 10
      if (w.autorun) {
        w.confirm = true
        w.stage.doTrack = false
      }
    } else if (input.keyDown(VK_S) && w.cooldown == 0) {
      val jfc : JFileChooser = new JFileChooser
      input.keys(VK_S) = false
      w.cooldown = 6
      if (jfc.showSaveDialog(Main.frame) == JFileChooser.APPROVE_OPTION)
        try {
          val bw : BufferedWriter = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(jfc.getSelectedFile), "UTF-8"))
          bw.write(w.sg.describe)
          bw.write("\nHISTORY:\n")
          for (s <- w.sg.log) {
            bw.write(s)
            bw.write('\n')
          }
          bw.close
        } catch {
          case e:Exception => JOptionPane.showMessageDialog(null, "Error: " + e.getMessage)
        }
    }
  }

}
