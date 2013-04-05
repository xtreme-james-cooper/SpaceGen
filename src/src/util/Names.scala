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

object Names {
  val NTHS : List[String] = List("Zeroth", "First", "Second", "Third", "Fourth", "Fifth", "Sixth",
    "Seventh", "Eighth", "Ninth", "Tenth", "Eleventh", "Twelfth", "Thirteenth", "Fourteenth")

  def nth(n : Int) : String = if (n < NTHS.length) NTHS(n) else n + "."

  val COLORS : List[String] = List("Red", "Green", "Blue", "Orange", "Yellow", "Black", "White", "Purple", "Grey")
}
