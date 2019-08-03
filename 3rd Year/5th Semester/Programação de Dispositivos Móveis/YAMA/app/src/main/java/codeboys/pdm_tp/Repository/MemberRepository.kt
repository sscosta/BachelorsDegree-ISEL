package codeboys.pdm_tp.Repository

import android.content.Context
import androidx.lifecycle.LiveData
import androidx.work.NetworkType
import androidx.work.PeriodicWorkRequestBuilder
import androidx.work.WorkManager
import codeboys.pdm_tp.MainRepository
import codeboys.pdm_tp.MessagingApp
import codeboys.pdm_tp.Workers.MemberWorker
import java.util.*
import androidx.work.*
import codeboys.pdm_tp.Model.Member

import java.util.concurrent.TimeUnit

class MemberRepository(private val mainRepository: MainRepository) {

    private val WORK_REQUEST_KEY = "Loading Members"
    private val LOAD_TEAMS_WORK_REQUEST_KEY = "LOAD_MEMBERS_WORK_REQUEST_ID "
    private val REPEAT_INTERVAL: Long = 24

    fun getMembers(idTeam: Int): LiveData<List<Member>> {
        if(workRequestIsNotSet(idTeam))
            setWorkRequest(idTeam)
        return (mainRepository as MessagingApp).roomDatabase.memberDao().getAllMembers(idTeam)
    }

    private fun setWorkRequest(idTeam: Int) {

        val data = Data.Builder()
        data.putInt("IdTeam", idTeam)

        val workRequest =
                PeriodicWorkRequestBuilder<MemberWorker>(REPEAT_INTERVAL, TimeUnit.HOURS)
                        .setConstraints(
                                Constraints.Builder()
                                        .setRequiredNetworkType(NetworkType.CONNECTED)
                                        .build()
                        )
                        .setInputData(
                                data.build()
                        )
                        .build()

        putWorkIdInSharedPrefs(workRequest.id,idTeam)

        WorkManager.getInstance().enqueue(workRequest)
    }

    private fun workRequestIsNotSet(idTeam: Int): Boolean {
        return (mainRepository as MessagingApp)
                .getSharedPreferences(
                        WORK_REQUEST_KEY,
                        Context.MODE_PRIVATE)
                .getString(LOAD_TEAMS_WORK_REQUEST_KEY + idTeam,null)==null
    }

    private fun putWorkIdInSharedPrefs(id: UUID, idTeam: Int) {
        val workDetails = (mainRepository as MessagingApp).getSharedPreferences(WORK_REQUEST_KEY, Context.MODE_PRIVATE)
        val edit = workDetails.edit()
        edit.putString(LOAD_TEAMS_WORK_REQUEST_KEY + idTeam ,id.toString())
        edit.apply()
    }
}