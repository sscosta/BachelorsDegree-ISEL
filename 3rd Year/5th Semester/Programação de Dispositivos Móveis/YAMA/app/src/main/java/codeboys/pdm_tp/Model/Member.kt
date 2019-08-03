package codeboys.pdm_tp.Model

import androidx.lifecycle.LiveData
import androidx.room.*

@Entity(tableName = "member")
data class Member(
    val login : String,
    @PrimaryKey val id : Int,
    val node_id : String,
    val avatar_url : String,
    val gravatar_id : String,
    val url : String,
    val html_url : String,
    val followers_url : String,
    val following_url : String,
    val gists_url : String,
    val starred_url : String,
    val subscriptions_url : String,
    val organizations_url : String,
    val repos_url : String,
    val events_url : String,
    val received_events_url : String,
    val type : String,
    val site_admin : Boolean,
    val idTeam: Int

)

@Dao
interface MemberDao {


    @Query("SELECT * FROM member where idTeam = :idTeam ORDER BY login ASC")
    fun getAllMembers(idTeam : Int) : LiveData<List<Member>>

    @Insert
    fun insert(member: Member)


    @Query("DELETE FROM member")
    fun deleteAll()

    @Query("DELETE FROM member where id = :idMember")
    fun delete(idMember : Int)

}