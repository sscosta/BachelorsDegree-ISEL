package codeboys.pdm_tp.Database

import androidx.room.Database
import androidx.room.RoomDatabase
import codeboys.pdm_tp.Model.*

@Database(entities = arrayOf(Team::class, Member::class), version = 1)
abstract class YamaDatabase : RoomDatabase() {
    abstract fun teamDao(): TeamDao
    abstract fun memberDao(): MemberDao
}