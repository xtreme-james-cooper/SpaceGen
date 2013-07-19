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

case class Tree[A](children : Set[A], parent : Option[A])

class Sprite(var img : BufferedImage, var x : Int, var y : Int, var tree : Tree[Sprite], var highlight : Boolean, var flash : Boolean) {

  def this(img : BufferedImage, x : Int, y : Int) = this(img, x, y, Tree(Set(), None), false, false)

  def globalX : Int = tree.parent match {
    case None    => x
    case Some(p) => p.globalX + x
  }

  def globalY : Int = tree.parent match {
    case None    => y
    case Some(p) => p.globalY + y
  }

  def attachToParent(parent : Sprite) : Unit = {
    tree = Tree(tree.children, Some(parent))
    parent.tree = Tree(parent.tree.children + this, parent.tree.parent)
  }

  def detachFromParent : Unit = {
    tree.parent match {
      case Some(p) => p.tree = Tree(p.tree.children - this, p.tree.parent)
      case None    => ()
    }
    tree = Tree(tree.children, None)
  }

}
