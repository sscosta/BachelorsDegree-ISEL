package codeboys.pdm_tp.Workers

import android.content.Context
import android.os.AsyncTask
import android.util.Log
import androidx.lifecycle.MutableLiveData
import androidx.work.*
import codeboys.pdm_tp.MessagingApp
import codeboys.pdm_tp.Model.Team
import codeboys.pdm_tp.Model.TeamDao
import com.android.volley.Request
import com.android.volley.RequestQueue
import com.android.volley.Response
import com.android.volley.toolbox.StringRequest
import org.json.JSONArray

class TeamWorker(private val context: Context, params: WorkerParameters) : Worker(context, params) {

    private val teamsDao = (context as MessagingApp).roomDatabase.teamDao()

    override fun doWork(): Result {
        addClearBD()
        updateTeams()
        return Result.SUCCESS
    }

    private fun addClearBD() {
        DeleteAllTeamsTask(teamsDao).execute()
    }

    private val requestQueue = (context as MessagingApp).requestQueue
    private fun updateTeams() {
        loadTeams(
            requestQueue,
            {insertAll(it)},
            { Log.d("update teams","Error on Update Teams: $it")}
        )
    }

    private val urlTeams = "https://api.github.com/orgs/isel-leic-pdm/teams"
    fun loadTeams(queue: RequestQueue, onSuccess : (List<Team>) -> Unit, onError : (String) -> Unit) {

        val stringRequest = object : StringRequest(
            Request.Method.GET,
            urlTeams,
            Response.Listener { response ->
                onSuccess(convertTeams(response))
            }, Response.ErrorListener { error ->
                onError(error.message ?: "error" )
            }) {
            override fun getHeaders(): MutableMap<String, String> {
                return mutableMapOf("Authorization" to (context as MessagingApp).repository.getProfileSharedData(context)!!.token)
            }
        }
        queue.add(stringRequest)
    }

    private fun convertTeams(response: String): List<Team> {
        val result = mutableListOf<Team>()
        var teamsJSONArray = JSONArray(response)

        for (jsonIndex in 0..(teamsJSONArray.length()-1)) {
            var teamObject = teamsJSONArray.getJSONObject(jsonIndex)
            result.add(
                Team(
                    teamObject.getString("name"),
                    teamObject.getInt("id"),
                    teamObject.getString("url"),
                    teamObject.getString("members_url")
                )
            )
        }
        return result
    }

    private fun insertAll(teams: List<Team>) {
        InsertTeamTask(teamsDao).execute(teams)
    }

    private fun checkDiffToLocal(remote : List<Team>) {
        val local = teamsDao.getAllTeams().value
        if(local==null && remote!=null)
            insertAll(remote)
        else if(local!=null){
            val toInsert = local?.let { remote.minus(it) }
            val toRemove = remote?.let{local.minus(it)}
            InsertTeamTask(teamsDao).execute(toInsert)
            DeleteTeamTask(teamsDao).execute(toRemove)
        }
    }
}

class InsertTeamTask(private val teamsDao: TeamDao) : AsyncTask<List<Team>, Void, Unit>() {
    override fun doInBackground(vararg teams: List<Team>) {
        for(team in teams[0])
            teamsDao.insert(team)
    }
}

class DeleteTeamTask(private val teamsDao: TeamDao) : AsyncTask<List<Team>, Void, Unit>() {
    override fun doInBackground(vararg team: List<Team>) {
        for (elem in team[0])
        teamsDao.delete(elem)
    }
}
class DeleteAllTeamsTask(private val teamsDao: TeamDao) : AsyncTask<List<Team>, Void, Unit>() {
    override fun doInBackground(vararg teams: List<Team>) {
        teamsDao.deleteAll()
    }
}