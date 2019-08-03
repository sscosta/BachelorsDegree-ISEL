package codeboys.pdm_tp.Workers

import android.content.Context
import android.os.AsyncTask
import android.widget.Toast
import androidx.work.Worker
//import androidx.work.Result
import androidx.work.WorkerParameters
import codeboys.pdm_tp.MessagingApp
import codeboys.pdm_tp.Model.Member
import codeboys.pdm_tp.Model.MemberDao
import com.android.volley.RequestQueue
import com.android.volley.Response
import com.android.volley.toolbox.StringRequest
import org.json.JSONArray
import com.android.volley.Request


class MemberWorker(private val context: Context, private val params: WorkerParameters) : Worker(context, params) {

    private val membersDao = (context as MessagingApp).roomDatabase.memberDao()
    private val requestQueue = (context as MessagingApp).requestQueue
    private var idTeam = 0

    override fun doWork(): Result {
        this.idTeam = params.inputData.getInt("IdTeam", 0)
        //addClearBD()
        updateMembers()
        return Result.SUCCESS
    }

    private fun updateMembers() {
        //addClearBD()
        loadMembers(requestQueue,
                { checkDiffToLocal(it) },
                { Toast.makeText(context as MessagingApp, "Error on Update Members: $it", Toast.LENGTH_LONG).show() }
        )

    }

    private fun insertAll(members: List<Member>) {
        InsertMemberTask(membersDao).execute(members)
    }


    private fun addClearBD() {
        DeleteAllMemberTask(membersDao).execute()
    }

    fun loadMembers(queue: RequestQueue, onSuccess: (List<Member>) -> Unit, onError: (String) -> Unit) {
        val stringRequest = object : StringRequest(
                Request.Method.GET,
                "https://api.github.com/teams/$idTeam/members",
                Response.Listener { response ->
                    onSuccess(convertMembers(response))
                }, Response.ErrorListener { error ->
            onError(error.message ?: "error")
        }) {
            override fun getHeaders(): MutableMap<String, String> {
                return mutableMapOf("Authorization" to (context as MessagingApp).repository.getProfileSharedData(context)!!.token)
            }
        }
        queue.add(stringRequest)
    }


    private fun convertMembers(response: String): List<Member> {
        val result = mutableListOf<Member>()
        var teamsJSONArray = JSONArray(response)

        for (jsonIndex in 0..(teamsJSONArray.length() - 1)) {
            var profileObject = teamsJSONArray.getJSONObject(jsonIndex)
            result.add(
                    Member(
                            profileObject.getString("login"),
                            profileObject.getInt("id"),
                            profileObject.getString("node_id"),
                            profileObject.getString("avatar_url"),
                            profileObject.getString("gravatar_id"),
                            profileObject.getString("url"),
                            profileObject.getString("html_url"),
                            profileObject.getString("followers_url"),
                            profileObject.getString("following_url"),
                            profileObject.getString("gists_url"),
                            profileObject.getString("starred_url"),
                            profileObject.getString("subscriptions_url"),
                            profileObject.getString("organizations_url"),
                            profileObject.getString("repos_url"),
                            profileObject.getString("events_url"),
                            profileObject.getString("received_events_url"),
                            profileObject.getString("type"),
                            profileObject.getBoolean("site_admin"),
                            idTeam
                    )
            )
        }
        return result
    }

    private fun checkDiffToLocal(remote : List<Member>) {
        val local = membersDao.getAllMembers(idTeam).value
        if(local==null&&remote!=null)insertAll(remote)
        else if(local!=null){
            val toInsert = local?.let { remote.minus(it) }
            val toRemove = remote?.let{local.minus(it)}
            InsertMemberTask(membersDao).execute(toInsert)
            DeleteMemberTask(membersDao).execute(toRemove)
        }
    }

}

class InsertMemberTask(private val membersDao: MemberDao) : AsyncTask<List<Member>, Void, Unit>() {
    override fun doInBackground(vararg members: List<Member>) {
        for (member in members[0])
            membersDao.insert(member)
    }
}

class DeleteAllMemberTask(private val membersDao: MemberDao) : AsyncTask<Member, Void, Unit>() {
    override fun doInBackground(vararg members: Member) {
        membersDao.deleteAll()
    }
}

class DeleteMemberTask(private val membersDao: MemberDao) : AsyncTask<List<Member>, Void, Unit>() {
    override fun doInBackground(vararg members: List<Member>) {
        for (member in members[0])
            membersDao.delete(member.id)
    }
}

