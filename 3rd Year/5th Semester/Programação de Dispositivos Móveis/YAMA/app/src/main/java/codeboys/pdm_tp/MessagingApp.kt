package codeboys.pdm_tp

import android.app.Application
import android.content.SharedPreferences
import androidx.room.Room
import codeboys.pdm_tp.Database.YamaDatabase
import codeboys.pdm_tp.Model.MessageSummary
import codeboys.pdm_tp.Repository.Repository
import com.android.volley.toolbox.Volley
import java.util.*

class MessagingApp : Application() , MainRepository{
    val roomDatabase by lazy {
        Room.databaseBuilder(this, YamaDatabase::class.java, "yama_db").build()
    }

    val repository by lazy {
        Repository(this)
    }

    val requestQueue by lazy { Volley.newRequestQueue(this) }

    val messageList = mutableListOf<MessageSummary>()

    val deviceId by lazy { getDeviceId(getSharedPreferences("prefs",0)) }

    private val DEV_ID = "DEV_ID"

    private fun getDeviceId(sharedPreferences: SharedPreferences) = sharedPreferences
        .getString(DEV_ID, null) ?: generateDeviceId(sharedPreferences)

    private fun generateDeviceId(sharedPreferences: SharedPreferences) : String {
        val devId=UUID.randomUUID().toString()
        sharedPreferences
            .edit()
            .putString(DEV_ID, devId)
            .apply()
        return devId
    }
}

val Application.requestQueue
    get() = (this as MessagingApp).requestQueue


val Application.repository
    get() = (this as MessagingApp).repository

val Application.messageList
    get() = (this as MessagingApp).messageList

