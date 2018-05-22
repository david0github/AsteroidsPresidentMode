/****
* author@ David Nguyen,Ryan Foch,Robert Laudadio,Matt Vacco,Alex Levin,Francisco Morales, and Geraldine Genuino
* professor@ Professor Duncan
* version@ Final Project: Asteroids
* class@ CSC110
* date@ May 13,2016
**/

import fang2.core.Game
import fang2.core.Sound
import java.awt.Color
import fang2.sprites._
import fang2.core.Sprite
import fang2.core.GameLevel
import fang2.core.AlarmAdapter

class AsteroidsMiniBoss extends Game(500,500) {
  val ship = new ImageSprite("galaga_ship.png");
  var numEnemies = 2
  var enemy: Array[ImageSprite] = Array.tabulate(numEnemies)(x => new ImageSprite("miniboss.png"))
  var numAs = 3
  var asteroids: Array[ImageSprite] = Array.tabulate(numAs)(x => new ImageSprite("trump_head.png"))
  var bullets: List[Bullet] = Nil 
  var vec: (Double, Double) = (0.0,0.0)
  var playerSpeed = 0.05
  var scoreSprite:StringSprite = new StringSprite("Score: 0")
  var score: Int = 0
  var miniboss2Health = 20
  var enemyDir: Array[(Double,Double)] = Array.tabulate(numEnemies)(x => ((math.random-0.5)/200.0, math.random/200.0))
  //var introSound: Sound = new Sound("start_game.wav")
  val DEFAULT_TIME_REMAINING = 40
  var timeRemaining = DEFAULT_TIME_REMAINING
  val timeSprite = new StringSprite("Time: " + timeRemaining)
  var minibossHealthBar:StringSprite = new StringSprite("Hillaryenator")
  
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
  
  override def setup {
   
    // addGame(new )
    //addGame(new )
    
    scoreSprite.setHeight(0.05)  
    scoreSprite.topJustify()     // Location specifies top edge of string spr.
    scoreSprite.leftJustify()    // Location specifies left edge of sprite
    scoreSprite.setLocation(0,0) // Set location (upper left corner)
    addSprite(scoreSprite)
    
    minibossHealthBar.setHeight(0.05)
    minibossHealthBar.setLocation(0.5,0.3)
    addSprite(minibossHealthBar)
    
    setHelpText("To be able to move, you must pressed the arrow keys for directions and the 'a' key to rotate the ship to the left, and the 'd' key to rotate the ship to the right.")
    // playSoundImmediately()
    // Sound.turnAllSoundOn()
    // introSound.play(1.0)
    ship.setLocation(0.5,0.9)
    ship.setScale(0.1)
    addSprite(ship)
    
    timeSprite.setHeight(0.05)  
    timeSprite.topJustify()     // Location specifies top edge of string spr.
    timeSprite.leftJustify()    // Location specifies right edge of sprite
    timeSprite.setLocation(0.80,0.01) // Set location (upper left corner)
    addSprite(timeSprite)
    
    for (e <- enemy.indices) {
      enemy(e).setScale(0.2)
      addSprite(enemy(e))
      setEnemyLocation(e) 
    }
    
    for (a <- asteroids.indices){
      asteroids(a).setScale(0.09)
      addSprite(asteroids(a))
      setAsteroidLocation(a)
    }
    
    schedule(new CountDown(), 1)
  }
  
  def moveShip {
    if(keyPressed){ 
      val c = getKeyPressed
      c match {
        case 'a' => ship.rotateRadians(-0.3)
        case 'd' => ship.rotateRadians(0.3)
        case 'g' => addGame(new AsteroidsTrumpWall)
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
  
  def wrap1 {
    for (e <- enemy.indices){
      if (enemy(e).getX > 1)
        enemy(e).setX(0)
      else if (enemy(e).getX < 0)
        enemy(e).setX(1)
      if (enemy(e).getY > 1)
        enemy(e).setY(0)
      else if (enemy(e).getY < 0)
        enemy(e).setY(1)
    }
  }

  
  
  override def advance() {
    moveShip
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
    
    for(e<- enemy.indices){
      if (ship.intersects(enemy(e))) {
        handleCollision
      }
    }
    
    for (e <- enemy.indices){ 
      moveEnemy(e)
      
    }
    for (e <- enemy.indices; b <- bullets.indices){
      moveBullet(e,b)
    }
    for (a <- asteroids.indices; b <- bullets.indices){
      moveTrump(a,b)
    }
 
  }
  
  
  
  def handleCollision {
    println("Game Over!")
    //startOver()
   finishGame()
  }
  
  def handleBulletCollision(e:Int, b:Int) {
    updateMiniHealth2(-1)
    bullets(b).setLoction(-5.0,-5.0)
    if(miniboss2Health == 0){
       println("GOOD JOB!!!!!")
    updateScore(+10)
    addGame(new AsteroidsTrumpWall)
    finishGame()
    }
    // explosionSound.play(1.0)
  }
  def handleBulletCollisionShip(b:Int){
    println("MAhAHahHA better luck next time!!")
    ship.setLocation(-9.0,-9.0)
     //startOver()
    finishGame()
  }
  
  def handleBulletCollisionTrump(a:Int, b:Int) {
    println("Nice,got one!")
    asteroids(a).setLocation(-4.0,-4.0)
    bullets(b).setLoction(-2.0,-2.0)
    updateScore(+5)
  }
  
  
  def moveEnemy(e:Int) {
    for (e <- enemy.indices) {
      enemy(e).translateX(enemyDir(e)._1)
      enemy(e).translateY(enemyDir(e)._2)
    }
    wrap1
  }
    
  
  
  def moveBullet(e:Int, b: Int){
    for (e<- enemy.indices; b <- bullets.indices){
      if (bullets(b).intersects(enemy(e))){
        handleBulletCollision(e, b)
      }  
    }
  }
  
  def moveTrump(a:Int, b:Int){
    for (a <- asteroids.indices; b <- bullets.indices){
      if (bullets(b).intersects(asteroids(a))){
        handleBulletCollisionTrump(a, b)
      }
    }
  }
  
  def setAsteroidLocation(a:Int) {
    for (a<- asteroids.indices){
      // asteroids(a).setLocation(math.random, math.random)
      asteroids(0).setLocation(0.7,0.2)
      asteroids(1).setLocation(0.6,0.8)
      asteroids(2).setLocation(0.3,0.3)
    }
  }
  
  def setEnemyLocation(e:Int) {
    for (e<- enemy.indices){
      enemy(e).setLocation((math.random*50).toInt/50.0,(math.random*5).toInt/20.0)
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
  def updateMiniHealth2(amount: Int) {
   miniboss2Health += amount 
   minibossHealthBar.setText("Hillaryenator: " + miniboss2Health)
  }
}

