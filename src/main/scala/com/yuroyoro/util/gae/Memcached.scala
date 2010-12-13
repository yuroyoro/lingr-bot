package com.yuroyoro.lingr

import scala.collection.JavaConversions._
import com.google.appengine.api.memcache._

// GAE MemcacheServiceのwrapper
trait Memcached[A <: AnyRef, B <: AnyRef] {
  val cache:MemcacheService = MemcacheServiceFactory.getMemcacheService
  implicit val DefaultExpiration = Expiration.byDeltaSeconds( 60*60*24*29 )
  implicit val DefaultPolicy = MemcacheService.SetPolicy.SET_ALWAYS

  def update(key:A, value:B) = put(key, value)
  def apply(key:A):Option[B] = Option(get(key))

  def put(key:A, value:B)(implicit expires:Expiration, policy:MemcacheService.SetPolicy) = cache.put(key ,value, expires, policy)
  def get(key:A):B = cache.get(key).asInstanceOf[B]
  def contains_?(key:A) = cache.contains(key)
  def all(keys:A*) = cache.getAll( keys )
  def statistics = cache.getStatistics
  def increment(key:A, delta:Long ) = cache.increment(key, delta)
  def putAll(values:Map[A, B] )(implicit expires:Expiration, policy:MemcacheService.SetPolicy) = cache.putAll(values, expires, policy)


}

object Base64Cache extends Memcached[String, Array[Byte]] {
  import com.yuroyoro.util.net.Base64

  override def put(key:String, value:Array[Byte])(implicit expires:Expiration, policy:MemcacheService.SetPolicy) = cache.put(key ,Base64.encodeBytes(value), expires, policy)
  override def get(key:String):Array[Byte] = {
    cache.get(key) match {
      case null => null
      case s => Base64.decodeBytes(s.asInstanceOf[String])
    }
  }
}

