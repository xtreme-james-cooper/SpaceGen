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

import src.SpaceGen
import src.util.Stage

class GameWorld {

  val sg : SpaceGen = new SpaceGen(System.currentTimeMillis)
  var stage : Stage = new Stage
  private var sx : Int = 0
  private var sy : Int = 0
  var cooldown : Int = 0
  var confirm : Boolean = false
  var confirmNeeded : Boolean = false
  var autorun : Boolean = false
  private var confirmWait : Int = 0
  private var firstRun : Boolean = true

  def tick : Boolean = {
    if (cooldown > 0)
      cooldown = cooldown - 1
    if (firstRun) {
      sg.init
      firstRun = false
    } else
      sg.tick
    false
  }

  def subTick : Boolean = {
    if (cooldown > 0)
      cooldown = cooldown - 1
    if (confirmNeeded) {
      if (confirm) {
        confirmNeeded = false
        if (autorun)
          sg.clearTurnLogOnNewEntry = true
        else
          sg.turnLog = Nil
        confirmWait = 0
      } else if (autorun) {
        confirmWait = confirmWait + 1
        if (confirmWait > 6) {
          confirmNeeded = false
          sg.clearTurnLogOnNewEntry = true
          confirmWait = 0
        }
      }
      false
    } else
      stage.tick
  }

}
