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

package src.util

import java.awt.image.BufferedImage

class Sprite(var img : BufferedImage, var x : Int, var y : Int) {
  var children : Set[Sprite] = Set()
  var parent : Option[Sprite] = None
  var highlight : Boolean = false
  var flash : Boolean = false

  def globalX : Int = parent match {
    case None    => x
    case Some(p) => p.globalX + x
  }

  def globalY : Int = parent match {
    case None    => y
    case Some(p) => p.globalY + y
  }

}