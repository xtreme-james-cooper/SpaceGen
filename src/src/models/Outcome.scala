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

package src.models

import src.Civ

sealed abstract class Outcome {

  def desc(encountered : Civ, previousStatus : Outcome) : String

}

case object WAR extends Outcome {
  override def desc(encountered : Civ, previousStatus : Outcome) : String = previousStatus match {
    case WAR   => "Peace negotiations with the " + encountered.name + " are unsuccessful and their war continues."
    case PEACE => "They declare war on the " + encountered.name + "!"
    case UNION => "???"
  }
}

case object PEACE extends Outcome {
  override def desc(encountered : Civ, previousStatus : Outcome) : String = previousStatus match {
    case WAR   => "They sign a peace accord with the " + encountered.name + ", ending their war."
    case PEACE => "They reaffirm their peaceful relations with the " + encountered.name + "."
    case UNION => "???"
  }
}

case object UNION extends Outcome {
  override def desc(encountered : Civ, previousStatus : Outcome) : String = previousStatus match {
    case WAR   => "In a historical moment, they put aside their differences with the " + encountered.name + ", uniting the two empires."
    case PEACE => "In a historical moment, they agree to combine their civilization with the " + encountered.name + "."
    case UNION => "???"
  }
}

