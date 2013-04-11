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

import src.Population

sealed abstract class DisappearanceCause
case object Transcended extends DisappearanceCause {
  override def toString : String = "transcended the bounds of this universe"
}
case class ByCataclysm(c : Cataclysm) extends DisappearanceCause {
  override def toString : String = "collapsed due to a " + c.name
}
case class ForReason(r : String) extends DisappearanceCause {
  override def toString : String = "collapsed " + r
}
case class ByPlague(p : Plague) extends DisappearanceCause {
  override def toString : String = "from the " + p.name //TODO needs a "collapsed"?
}

class Remnant(
  remnant : Population,
  collapseTime : Int,
  val cause : DisappearanceCause) extends Stratum(collapseTime) {

  override def toString : String = {
    val plagueStr : String = cause match {
      case ByPlague(p) => " The " + p.desc + ", slumbers in their corpses."
      case _           => ""
    }
    "Remnants of a culture of " + remnant.typ.name + " that " + cause + " in " + collapseTime + "." + plagueStr
  }

}
