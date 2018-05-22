/****
* author@ David Nguyen,Ryan Foch,Robert Laudadio,Matt Vacco,Alex Levin,Francisco Morales, and Geraldine Genuino
* professor@ Professor Duncan
* version@ Final Project: Asteroids
* class@ CSC110
* date@ May 13,2016
**/

import fang2.sprites._
import fang2.core.Game
import fang2.core.Sound
import java.awt.Color
import fang2.core.Sprite
import fang2.core.GameLevel
import fang2.core.AlarmAdapter

class AsteroidsTrumpWall extends Game(500,500) {
   val ship = new ImageSprite("galaga_ship.png");
   var numWalls = 21
   var wall: Array[ImageSprite] = Array.tabulate(numWalls)(x => new ImageSprite("trump_head.png"))
   var bullets: List[Bullet] = Nil
   var vec: (Double, Double) = (0.0,0.0)
   var playerSpeed = 0.05
   var scoreSprite:StringSprite = new StringSprite("Score: 0")
   var score: Int = 0
   var trumpWall = new ImageSprite("trump_head.png")
   var trumpSpeed = 0.05
   val DEFAULT_TIME_REMAINING = 60
   var trumpHealth = 30
   var timeRemaining = DEFAULT_TIME_REMAINING
   val timeSprite = new StringSprite("Time: " + timeRemaining)
   var trumpHealthBar:StringSprite = new StringSprite("Trumpenator")
   
   class CountDown extends AlarmAdapter {
    override def act {	
      timeRemaining -= 1   // Decrease the amount of time remaining
      if (timeRemaining <= 0) {
        // Time is up - end the game...
        timeRemaining = 0
        finishGame()
      } else {
        schedule(this, 1) 
      }
      timeSprite.setText("Time: " + timeRemaining)  // Update the sprite
    }
  }
  
  override def setup{
    
    scoreSprite.setHeight(0.05)  
    scoreSprite.topJustify()     // Location specifies top edge of string spr.
    scoreSprite.leftJustify()    // Location specifies left edge of sprite
    scoreSprite.setLocation(0,0) // Set location (upper left corner)
    addSprite(scoreSprite)
    
    trumpHealthBar.setHeight(0.05)
    trumpHealthBar.setLocation(0.5,0.3)
    addSprite(trumpHealthBar)
    
    setHelpText("To be able to move, you must pressed the arrow keys for directions and the 'a' key to rotate the ship to the left, and the 'd' key to rotate the ship to the right.")
  
    ship.setLocation(0.5,0.9)
    ship.setScale(0.1)
    addSprite(ship)
  
    trumpWall.setLocation(0.5,0.1)
    trumpWall.setScale(0.3)
    addSprite(trumpWall)
    
    timeSprite.setHeight(0.05)  
    timeSprite.topJustify()     // Location specifies top edge of string spr.
    timeSprite.leftJustify()    // Location specifies right edge of sprite
    timeSprite.setLocation(0.80,0.01) // Set location (upper left corner)
    addSprite(timeSprite)
  
    for (w <- wall.indices){
      wall(w).setScale(0.09)
      addSprite(wall(w))
      setWallLocation(w)
    }
    
    schedule(new CountDown(), 1)
  }
  
  def moveShip {
    if(keyPressed){ 
      val c = getKeyPressed
      c match {
        case 'a' => ship.rotateRadians(0.3)
        case 'd' => ship.rotateRadians(-0.3)
        case 'g' => addGame(new AsteroidsBoss) 
        finishGame()
        case _ =>  
      }
    }
    if (leftPressed) {
      ship.translateX(-playerSpeed*1.6)
    } else if (rightPressed) {
      ship.translateX(playerSpeed*1.6)
    } else if (upPressed) {
      ship.translateY(-playerSpeed*1.6)
    } else if (downPressed) {
      ship.translateY(playerSpeed*1.6)
    } 
    wrap
  }
  
  def moveTrump {
    if(keyPressed){ 
      val c = getKeyPressed
      c match {
        case 'a' => trumpWall.rotateRadians(-0.3)
        case 'd' => trumpWall.rotateRadians(0.3)
        case _ =>  
      }
    }
    if (leftPressed) {
      trumpWall.translateX(-trumpSpeed*1)
    } else if (rightPressed) {
      trumpWall.translateX(trumpSpeed*1)
    } else if (upPressed) {
      trumpWall.translateY(trumpSpeed*1)
    } else if (downPressed) {
      trumpWall.translateY(-trumpSpeed*1)
    } 
    wrapTrump
  }
  
  
   def wrap {
    if (ship.getX > 1)
      ship.setX(0)
    else if (ship.getX < 0)
      ship.setX(1)
    if (ship.getY > 1)
      ship.setY(0)
    else if (ship.getY < 0)
      ship.setY(1)
  }
  
  def wrapTrump {
    if ( trumpWall.getX > 1)
       trumpWall.setX(0)
    else if ( trumpWall.getX < 0)
       trumpWall.setX(1)
    if ( trumpWall.getY > 1)
       trumpWall.setY(0)
    else if ( trumpWall.getY < 0)
      trumpWall.setY(1)
  }
  
  override def advance {
    moveShip
    moveTrump
    // See if the user pressed a space (fire a bullet!)
    if (keyPressed && getKeyPressed == ' ') {
      // Make a new Bullet
      val bullet = new Bullet(new ImageSprite("praying.png"), ship.getRotation)
      // Set the Bullet location to the center of the ship
      bullet.setLoction(ship.getX,ship.getY)
      // Add the Bullet to list, and to the game
      bullets = bullet::bullets
      addSprite(bullet.sprite)
    }
     // Move any Bullets in the list (until they leave the screen
    if (bullets != null) {
      // Use a higher-order function to split the list into
      // visible and not-visible Lists.
      val (bullet1,bullet2) = bullets.partition((x: Bullet) => {
        x.moveShip
        x.sprite.isVisible
      })
      // Save the visible Bullets back in bullets
      bullets = bullet1
      // Remove the Bullets no longer visible
      bullet2.foreach((x: Bullet) => removeSprite(x.sprite))
    }
    for (w <- wall.indices){
      if (ship.intersects(wall(w))){
        handleCollision
      }
    }
    for (b <- bullets.indices) {
      moveTrump(b)
    }
    for (w <- wall.indices; b <- bullets.indices){
      moveBullet(w,b)
    }
  }
  
  def handleCollision {
    println("CRASH INTO A WALL!! GAME OVER!!")
    finishGame()
  }
  
  def handleBulletCollision(w:Int, b:Int){
    wall(w).setLocation(-1.0,-1.0)
    bullets(b).setLoction(-2.2,-2.2)
    updateScore(+3)
  }
  
  def handleBulletCollisionTrump(b:Int){
    updateTrumpHealth(-2)
    bullets(b).setLoction(-1.9,-1.9)
    if (trumpHealth == 0){
    println("YOU HAVE CRASHED THE WALL!!")
    trumpWall.setLocation(-1.2,-1.2)
    updateScore(+20)
    addGame(new AsteroidsBoss)
    //startOver()
    finishGame()
    
    }
  }
  def moveBullet(w:Int, b:Int){
    for (w <- wall.indices; b <- bullets.indices){
      if(bullets(b).intersects(wall(w))){
        handleBulletCollision(w, b)
      }
    }
  }
  
  def moveTrump(b:Int){
    for(b <- bullets.indices){
      if (bullets(b).intersects(trumpWall)){
        handleBulletCollisionTrump(b)
      }
    }
  }
  
  def setWallLocation(w:Int){
    for( w <- wall.indices){
      wall(0).setLocation(0.0,0.5)
      wall(1).setLocation(0.1,0.5)
      wall(2).setLocation(0.2,0.5)
      wall(3).setLocation(0.3,0.5)
      wall(4).setLocation(0.4,0.5)
      wall(5).setLocation(0.5,0.5)
      wall(6).setLocation(0.6,0.5)
      wall(7).setLocation(0.7,0.5)
      wall(8).setLocation(0.8,0.5)
      wall(9).setLocation(0.9,0.5)
      wall(10).setLocation(1.0,0.5)
      wall(11).setLocation(0.15,0.6)
      wall(12).setLocation(0.25,0.6)
      wall(13).setLocation(0.35,0.6)
      wall(14).setLocation(0.45,0.6)
      wall(15).setLocation(0.55,0.6)
      wall(16).setLocation(0.65,0.6)
      wall(17).setLocation(0.75,0.6)
      wall(18).setLocation(0.85,0.6)
      wall(19).setLocation(0.05,0.6)
      wall(20).setLocation(0.95,0.6)
    }
  }
  
   /*****
  * updateScore
  *    Update the actual score AND the displayed score (text)
  *    using the amount provided
  *****/
  def updateScore(amount: Int) {
    score += amount   
    scoreSprite.setText("Score: " + score)
  }
  
  def updateTrumpHealth(amount: Int) {
    trumpHealth += amount   
    trumpHealthBar.setText("Trumpenator: " + trumpHealth)
  }
  
}