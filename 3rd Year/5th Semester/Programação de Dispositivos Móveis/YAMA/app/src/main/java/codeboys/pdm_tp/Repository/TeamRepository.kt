package codeboys.pdm_tp.Repository

import android.content.Context
import androidx.work.*
import codeboys.pdm_tp.MainRepository
import codeboys.pdm_tp.MessagingApp
import codeboys.pdm_tp.Workers.TeamWorker
import java.util.*
import java.util.concurrent.TimeUnit

class TeamRepository(private val mainRepository: MainRepository)
{
    private val WORK_REQUEST_KEY = "Loading Teams"
    private val LOAD_TEAMS_WORK_REQUEST_KEY = "LOAD_TEAMS_WORK_REQUEST_ID"

    val allTeams by lazy { (mainRepository as MessagingApp).roomDatabase.teamDao().getAllTeams() }

    fun loadTeams(){
        if(workRequestIsNotSet())
            setWorkRequest()
    }

    private fun workRequestIsNotSet(): Boolean {
        return (mainRepository as MessagingApp)
                .getSharedPreferences(
                        WORK_REQUEST_KEY,
                        Context.MODE_PRIVATE)
                .getString(LOAD_TEAMS_WORK_REQUEST_KEY, null)==null
    }


    private val REPEAT_INTERVAL : Long = 24
    private fun setWorkRequest() {
        val workRequest =
                PeriodicWorkRequestBuilder<TeamWorker>(REPEAT_INTERVAL,TimeUnit.HOURS)
                        .setConstraints(
                                Constraints.Builder()
                                        .setRequiredNetworkType(NetworkType.CONNECTED)
                                        .build()
                        )
                        .build()

        putWorkIdInSharedPrefs(workRequest.id)

        WorkManager.getInstance().enqueue(workRequest) //enqueueUniquePeriodicWork
    }
    private fun putWorkIdInSharedPrefs(id: UUID) {
        val workDetails = (mainRepository as MessagingApp).getSharedPreferences(WORK_REQUEST_KEY, Context.MODE_PRIVATE)
        val edit = workDetails.edit()
        edit.putString(LOAD_TEAMS_WORK_REQUEST_KEY,id.toString())
        edit.apply()
    }
}

