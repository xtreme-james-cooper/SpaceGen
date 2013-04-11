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

import src.util.Sprite
import src.Agent
import src.Civ
import src.SentientType
import java.awt.image.BufferedImage
import src.util.MediaProvider
import java.awt.Graphics2D
import java.awt.Transparency

sealed abstract class Container {
  def getSprite : BufferedImage
}
case class ContainedST(st : SentientType) extends Container {
  def getSprite : BufferedImage = st.getSprite(false, false)
}
case class ContainedArtefact(st : Artefact) extends Container {
  def getSprite : BufferedImage = st.getSprite
}
case class ContainedAgent(st : Agent) extends Container {
  def getSprite : BufferedImage = st.getSprite
}

class Artefact(
  created : Int,
  val creator : Option[Civ],
  val typ : ArtefactType,
  var desc : String,
  val st : Option[SentientType],
  val creatorTechLevel : Int,
  var specialValue : Int,
  creatorName : Option[String],
  var contained : Option[Container]) {

  def this(created : Int, creator : Option[Civ], typ : ArtefactType, desc : String) = this(
    created, creator, typ, desc,
    if (creator.isDefined) creator.get.fullMembers.headOption else None,
    if (creator.isDefined) creator.get.techLevel else 0,
    0, None, None)

  def this(created : Int, creatorName : String, typ : ArtefactType, desc : String) = this(
    created, None, typ, desc, None, 0, 0, Some(creatorName), None)

  override def toString : String = desc + (creatorName match {
    case Some(cName) => " created by " + cName + " in " + created
    case None => creator match {
      case Some(c) if typ != WRECK => " created by the " + c.name + " in " + created
      case _                   => ""
    }
  })

  def getSprite : BufferedImage = MediaProvider.border(typ.getSprite(contained))

}
