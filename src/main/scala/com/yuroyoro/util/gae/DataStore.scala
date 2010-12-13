package com.yuroyoro.util.gae

import scala.collection.JavaConversions._
import com.google.appengine.api.datastore._
import com.google.appengine.api.datastore.Query.{FilterOperator, SortDirection }

// GAE DatastoreServiceのwrapper
trait DataStoreSupport {
  private def ds =  DatastoreServiceFactory.getDatastoreService

  // データストアに対するトランザクションを開始します。
  def beginTransaction():Option[Transaction] = Option(ds.beginTransaction)

  // keys で指定された Entity entities を削除します。
  def delete(keys:Key*)(implicit tx:Option[Transaction] = None) = tx match{
    case Some(txn) => ds.delete(txn, keys:_*)
    case None => ds.delete( keys:_*)
  }

  // 一連の Entities 一致 keys を取得します。
  def getAll(keys:Key*)(implicit tx:Option[Transaction] = None):Map[Key, Option[Entity]] = (tx match{
    case Some(txn) => ds.get(txn, keys)
    case None => ds.get( keys)
  }).map{ case (k, v) => (k, Option(v)) }.toMap

  // 指定された Key を持つ Entity を取得します。
  def get(key:Key)(implicit tx:Option[Transaction] = None ):Option[Entity] = {
   import scala.util.control.Exception._

    allCatch opt {tx match{
      case Some(txn) => ds.get(txn, key)
      case None => ds.get( key)
    }}
  }

  def activeTransactions = ds.getActiveTransactions

  // このスレッドの現在のトランザクションを返します。
  // 現在のトランザクションがない場合はパラメータを返します。
  def currentTransaction( tx:Option[Transaction] = None) = {
    import scala.util.control.Exception._
    tx match {
      case Some(tx) => allCatch.opt{ ds.getCurrentTransaction(tx)}
      case None => allCatch.opt{ ds.getCurrentTransaction}
    }
  }

  // クエリを実行する準備をします。
  def prepare(query:Query):PreparedQuery = ds.prepare(query)

  // 指定された Entity がデータ ストアにない場合は、作成してその Key を割り当てます。
  def put(entity:Entity)(implicit tx:Option[Transaction] = None ):Key = tx match {
    case Some(txn) => ds.put(txn, entity)
    case None => ds.put(entity)
  }

  // すべての entities のバッチ put を実行します。
  def putAll(entities:Entity*)(implicit tx:Option[Transaction] = None ):Seq[Key] = (tx match {
    case Some(txn) => ds.put(txn, entities)
    case None => ds.put(entities)
  }).toSeq

   def createEntity(kind:String):Entity = new Entity(kind)
   def createEntity(kind:String, parent:Key):Entity = new Entity(kind, parent)
   def createEntity(kind:String, keyname:String):Entity = new Entity(kind, keyname)
   def createEntity(kind:String, keyname:String, parent:Key):Entity = new Entity(kind, keyname, parent)

   def createKey( kind:String, id:Long):Key = KeyFactory.createKey(kind, id)
   def createKey( kind:String, name:String):Key = KeyFactory.createKey(kind, name)
   def createKey( parent:Key, kind:String, id:Long):Key = KeyFactory.createKey(parent, kind, id)
   def createKey( parent:Key, kind:String, name:String):Key = KeyFactory.createKey(parent, kind, name)

  /**
   * GAEのEnityがもつPropertyの値を型を指定してとりだします。
   */
  def entityValue[T]( e:Entity, name:String ) =
    e.getProperty( name ).asInstanceOf[T]
}

object DataStore extends DataStoreSupport

