package codeboys.pdm_tp.Model

import androidx.lifecycle.LiveData
import androidx.room.*

@Entity(tableName = "team")
data class Team(
    @PrimaryKey val name : String,
    val id : Int,
    val url : String ,
    val membersUrl : String
)

@Dao
interface TeamDao {

    @Query("SELECT * FROM team ORDER BY name ASC")
    fun getAllTeams() : LiveData<List<Team>>

    @Insert
    fun insert(team: Team)

    @Query("DELETE FROM team")
    fun deleteAll()

    @Delete
    fun delete(vararg team: Team)
}