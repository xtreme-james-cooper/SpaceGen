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

object Prefix {

  val values : List[Prefix] = List(FLYING, TINY, GIANT, SIX_LEGGED, FOUR_ARMED, TWO_HEADED, SLIM, AMORPHOUS, FEATHERED, SCALY, IMMORTAL, TELEPATHIC)

}

sealed abstract class Prefix(val name : String, ssName : Option[String]) {
  val specialStruct : Option[StructureType] = for (s <- ssName) yield new SpecialStructureType(s)
}

case object FLYING extends Prefix("Flying", Some("grand roost"))
case object TINY extends Prefix("Tiny", None)
case object GIANT extends Prefix("Giant", None)
case object SIX_LEGGED extends Prefix("Six-Legged", None)
case object FOUR_ARMED extends Prefix("Four-Armed", None)
case object TWO_HEADED extends Prefix("Two-Headed", None)
case object SLIM extends Prefix("Slim", None)
case object AMORPHOUS extends Prefix("Amorphous", Some("reforming vat"))
case object FEATHERED extends Prefix("Feathered", None)
case object SCALY extends Prefix("Scaly", None)
case object IMMORTAL extends Prefix("Immortal", None)
case object TELEPATHIC extends Prefix("Telepathic", None)

