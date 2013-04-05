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

class LostArtefact(status : String, lostTime : Int, val artefact : Artefact) extends Stratum(lostTime) {

  override def toString : String =
    artefact.typ match {
      case PIRATE_TOMB | ADVENTURER_TOMB => "The " + artefact + ", buried in " + lostTime + "."
      case WRECK                         => "The " + artefact + "."
      case _                             => "A " + artefact + ", " + status + " in " + lostTime + "."
    }

}
