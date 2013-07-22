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

import java.awt.Canvas
import java.awt.Cursor
import java.awt.Point
import java.awt.Toolkit
import java.awt.image.BufferedImage
import javax.swing.JFrame
import src.util.Stage
import src.util.Animation
import src.util.MediaProvider

object Main {
  val width : Int = 800
  val height : Int = 600
  val frame : JFrame = new JFrame("SpaceGen")
  frame.setIgnoreRepaint(true)
  frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)

  // Transparent 16 x 16 pixel cursor image.
  val cursorImg : BufferedImage = new BufferedImage(16, 16, BufferedImage.TYPE_INT_ARGB)

  val c : Canvas = new Canvas
  c.setCursor(null)
  c.setIgnoreRepaint(true)
  frame.add(c)
  frame.setSize(width, height)
  frame.setResizable(false)
  frame.setVisible(true)
  MediaProvider.createInstance(c.getGraphicsConfiguration)
  val w : GameWorld = new GameWorld
  val d : GameDisplay = new GameDisplay(w, width, height)
  c.createBufferStrategy(2)
  val input : Input = new Input
  c.addKeyListener(input)
  c.addMouseListener(input)
  c.addMouseMotionListener(input)
  c.requestFocus
  val gt : GameThread = new GameThread(input, d, new GameControls(d, w, input), c.getBufferStrategy)

  def confirm : Unit = {
    w.confirmNeeded = true //TODO drop
    w.confirm = false //TODO drop
    w.stage.doTrack = !w.autorun
    gt.run(_ => w.subTick)
  }

  def animate(as : Animation) : Unit = {
    w.stage.animate(as)
    gt.run(_ => w.subTick)
  }

  def animate(as : Animation, as2 : Animation) : Unit = {
    w.stage.animate(as, as2)
    gt.run(_ => w.subTick)
  }

  def add(a : Animation) : Unit = w.stage.animate(a)

  def animate : Unit = gt.run(_ => w.subTick)

  def main(args : Array[String]) : Unit = gt.run(_ => w.tick)
}
