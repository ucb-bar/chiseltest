// SPDX-License-Identifier: Apache-2.0

package treadle.executable

/** A RollBackBuffer is the an image of [[DataStore]] at a particular time.
  * @param dataStore the dataStore to be backed up.
  */
class RollBackBuffer(dataStore: DataStore) extends HasDataArrays {
  var time: Long = 0L

  val intData:  Array[Int] = Array.fill(dataStore.numberOfInts)(0)
  val longData: Array[Long] = Array.fill(dataStore.numberOfLongs)(0)
  val bigData:  Array[Big] = Array.fill(dataStore.numberOfBigs)(0)

  def dump(dumpTime: Long): Unit = {
    time = dumpTime
    Array.copy(dataStore.intData, 0, intData, 0, intData.length)
    Array.copy(dataStore.longData, 0, longData, 0, longData.length)
    Array.copy(dataStore.bigData, 0, bigData, 0, bigData.length)
  }
}

/** Maintains a ring buffer of dataStore images
  * The only real complexity here is that the number of populated buffers is zero.
  * @param dataStore dataStore of project, used to determine size of ring
  */
class RollBackBufferRing(dataStore: DataStore) {
  val numberOfBuffers: Int = dataStore.numberOfBuffers
  val ringBuffer:      Array[RollBackBuffer] = Array.fill(numberOfBuffers)(new RollBackBuffer(dataStore))

  var oldestBufferIndex: Int = 0
  var latestBufferIndex: Int = 0

  def currentNumberOfBuffers: Int = {
    if (numberOfBuffers == 0) {
      0
    } else {
      (latestBufferIndex + numberOfBuffers - oldestBufferIndex) % numberOfBuffers
    }
  }

  /** Return the buffers as a list in reverse time order.
    * @return
    */
  def newestToOldestBuffers: Seq[RollBackBuffer] = {
    var list = List.empty[RollBackBuffer]
    if (currentNumberOfBuffers > 0) {
      var index = latestBufferIndex
      while (index != oldestBufferIndex) {
        list = list :+ ringBuffer(index)
        index -= 1
        if (index < 0) {
          index = numberOfBuffers - 1
        }
      }
      list = list :+ ringBuffer(index)
    }
    list
  }

  /** Advances the last buffer pointer and returns a buffer to be used for new data.
    * In the beginning this is an unused buffer, after the ring fills, it returns the oldest buffer
    * If the time parameter matches the most recent buffers time, that buffer will be re-used.
    * @param time the time that the returned buffer will be used to store d
    * @return
    */
  def advanceAndGetNextBuffer(time: Long): RollBackBuffer = {
    if (currentNumberOfBuffers > 0 && time < ringBuffer(latestBufferIndex).time) {
      // It's an error to record something earlier in time
      throw TreadleException(s"rollback buffer requested has earlier time that last used buffer")
    } else if (
      currentNumberOfBuffers == 0 || (currentNumberOfBuffers > 0 && time > ringBuffer(latestBufferIndex).time)
    ) {
      // time has advanced so get a new buffer or re-use the oldest one
      // if time did not advance just fall through and newest buffer to be used again
      latestBufferIndex += 1
      if (latestBufferIndex >= numberOfBuffers) {
        latestBufferIndex = 0
      }
      if (latestBufferIndex == oldestBufferIndex) {
        oldestBufferIndex += 1
        if (oldestBufferIndex >= numberOfBuffers) {
          oldestBufferIndex = 0
        }
      }
    }
    ringBuffer(latestBufferIndex)
  }
}

/** Manage the allocation of the rollback buffers
  */
class RollBackBufferManager(dataStore: DataStore) {

  val rollBackBufferRing = new RollBackBufferRing(dataStore)

  /** save current system state for a specific clock.
    * @param time the time that snapshot will be for
    */
  def saveData(time: Long): Unit = {
    val buffer = rollBackBufferRing.advanceAndGetNextBuffer(time)
    buffer.dump(time)
  }

  /** Finds the most recent buffer for the specified clock that is older than the specified time
    * @param time      a time that the buffer must be older than
    * @return
    */
  def findEarlierBuffer(time: Long): Option[RollBackBuffer] = {
    rollBackBufferRing.newestToOldestBuffers.find { buffer =>
      buffer.time < time
    }
  }

  /** Finds a buffer where a previous buffer has a high clock and this one has a low clock,then return this.
    *
    * @param time             buffer time must be earlier (<) than time
    * @param clockIndex       where to find value of clock at time rollback buffer's time
    * @param prevClockIndex   same as above but index is prevClock value
    * @return
    */
  def findBufferBeforeClockTransition(time: Long, clockIndex: Int, prevClockIndex: Int): Option[RollBackBuffer] = {
    var foundHighClock = false

    rollBackBufferRing.newestToOldestBuffers.find { buffer =>
      if (buffer.time >= time) {
        false
      } else if (foundHighClock && buffer.intData(prevClockIndex) == 0) {
        true
      } else if (buffer.intData(clockIndex) > 0) {
        foundHighClock = true
        false
      } else {
        foundHighClock = false
        false
      }
    }
  }

  /** returns a Seq of rollback buffers
    * @return
    */
  def newestToOldestBuffers: Seq[RollBackBuffer] = rollBackBufferRing.newestToOldestBuffers
}
