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

class AsteroidsBoss extends Game(500,500){
  val ship = new ImageSprite("galaga_ship.png");
  val numEnemies = 4
  var boss = new ImageSprite("boss_head.png");
  val bossSpeed = 0.05
  val enemy: Array[ImageSprite] = Array.tabulate(numEnemies)(x => new ImageSprite("galaga_dragonfly.png"))
  val numAs = 3
  val asteroids: Array[ImageSprite] = Array.tabulate(numAs)(x => new ImageSprite("trump_head.png"))
  var bullets: List[Bullet] = Nil 
  var bulletz: List[BulletBoss] = Nil
  var vec: (Double, Double) = (0.0,0.0)
  val playerSpeed = 0.05
  var scoreSprite:StringSprite = new StringSprite("Score: 0")
  var score: Int = 0
  var bossHealthBar:StringSprite = new StringSprite("Bernenator")
  var bossHealth = 20
  var enemyDir: Array[(Double,Double)] = Array.tabulate(numEnemies)(x => ((math.random-0.5)/200.0, math.random/200.0))
  //var introSound: Sound = new Sound("start_game.wav")
  val DEFAULT_TIME_REMAINING = 50
  var timeRemaining = DEFAULT_TIME_REMAINING
  val timeSprite = new StringSprite("Time: " + timeRemaining)
  
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
    
    //addGame(new )
    // addGame(new )
    //addGame(new )
    
    scoreSprite.setHeight(0.05)  
    scoreSprite.topJustify()     // Location specifies top edge of string spr.
    scoreSprite.leftJustify()    // Location specifies left edge of sprite
    scoreSprite.setLocation(0,0) // Set location (upper left corner)
    addSprite(scoreSprite)
    
    bossHealthBar.setHeight(0.05)  
    bossHealthBar.setLocation(0.5,0.3) // Set location (upper left corner)
    addSprite(bossHealthBar)
    
    
    setHelpText("To be able to move, you must pressed the arrow keys for directions and the 'a' key to rotate the ship to the left, and the 'd' key to rotate the ship to the right.")
    // playSoundImmediately()
    ship.setLocation(0.5,0.9)
    ship.setScale(0.1)
    addSprite(ship)
    
    boss.setLocation(0.5,0.1)
    boss.setScale(0.3)
    addSprite(boss)
    
    timeSprite.setHeight(0.05)  
    timeSprite.topJustify()     // Location specifies top edge of string spr.
    timeSprite.leftJustify()    // Location specifies right edge of sprite
    timeSprite.setLocation(0.80,0.01) // Set location (upper left corner)
    addSprite(timeSprite)
    
    for (e <- enemy.indices) {
      enemy(e).setScale(0.09)
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
        case 'a' => ship.rotateRadians(0.3)
        case 'd' => ship.rotateRadians(-0.3)
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
  
  def moveBoss {
    if(keyPressed){ 
      val c = getKeyPressed
      c match {
        case 'a' => boss.rotateRadians(-0.3)
        case 'd' => boss.rotateRadians(0.3)
        case 'g' => finishGame()
        case _ =>  
      }
    }
    if (leftPressed) {
      boss.translateX(-bossSpeed*1)
    } else if (rightPressed) {
      boss.translateX(bossSpeed*1)
    } else if (upPressed) {
      boss.translateY(bossSpeed*1)
    } else if (downPressed) {
      boss.translateY(-bossSpeed*1)
    } 
    wrapBoss
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
  
  def wrapBoss {
    if (boss.getX > 1)
      boss.setX(0)
    else if (boss.getX < 0)
      boss.setX(1)
    if (boss.getY > 1)
      boss.setY(0)
    else if (boss.getY < 0)
      boss.setY(1)
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
  override def advance {
    moveShip
    moveBoss
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
    
    if (keyPressed && getKeyPressed == ' ') {
      // Make a new Bullet
      val bullet = new BulletBoss(new ImageSprite("flyingmoney.png"), boss.getRotation)
      // Set the Bullet location to the center of the ship
      bullet.setLoction(boss.getX,boss.getY)
      // Add the Bullet to list, and to the game
      bulletz = bullet::bulletz
      addSprite(bullet.sprite)
    }
    if (bulletz != null) {
      // Use a higher-order function to split the list into
      // visible and not-visible Lists.
      val (bullet1,bullet2) = bulletz.partition((x: BulletBoss) => {
        x.moveBoss
        x.sprite.isVisible
      })
      // Save the visible Bullets back in bullets
      bulletz = bullet1
      // Remove the Bullets no longer visible
      bullet2.foreach((x: BulletBoss) => removeSprite(x.sprite))
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
    
    for (b <- bulletz.indices){
      moveShip(b)
    }
    for(b<- bullets.indices){
      moveBoss(b)
    }
    
  }
  def handleCollision {
    println("Game Over!")
    //startOver()
    finishGame()
  }
  
  def handleBulletCollision(e:Int, b:Int) {
    println("GOT YOU!!!")
    enemy(e).setLocation(-1.0,-1.0)
    bullets(b).setLoction(-5.0,-5.0)
    updateScore(+10)
    
    //explosionSound.play(1.0)
  }
  
  def handleBulletCollisionTrump(a:Int, b:Int) {
    println("Nice,got one!")
    asteroids(a).setLocation(-4.0,-4.0)
    bullets(b).setLoction(-2.0,-2.0)
    updateScore(+5)
  }
  
  def handleBulletCollisionShip(b:Int){
    println("MAhAHahHA better luck next time!!")
    ship.setLocation(-9.0,-9.0)
    bulletz(b).setLoction(-1.5,-1.5)
    //startOver()
    finishGame()
  }
  
  def handleBulletCollisionBoss(b:Int){
    updateBossHealth(-1)
    //boss.setLocation(-1.3,-1.3)
    bullets(b).setLoction(-1.8,-1.8)
    if (bossHealth == 0){
      println("YOU HAVE DEFEATED THE BOSS AND GAME!!!")
      boss.setLocation(-1.3,-1.3)
      //startOver()
      finishGame()
    }
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
  
  def moveShip(b:Int){
    for(b <- bulletz.indices){
      if(bulletz(b).intersects(ship)){
        handleBulletCollisionShip(b)
      }
    }
  }
  
  def moveBoss(b:Int){
    for(b <- bullets.indices){
      if(bullets(b).intersects(boss)){
        handleBulletCollisionBoss(b)
      }
    }
  }
  
  def setAsteroidLocation(a:Int) {
    for (a<- asteroids.indices){
      // asteroids(a).setLocation(math.random, math.random)
      asteroids(0).setLocation(0.2,0.1)
      asteroids(1).setLocation(0.8,0.2)
      asteroids(2).setLocation(0.27,0.3)
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
  
  def updateBossHealth(amount: Int) {
    bossHealth += amount   
    bossHealthBar.setText("Bernenator: " + bossHealth)
  }
  
}
