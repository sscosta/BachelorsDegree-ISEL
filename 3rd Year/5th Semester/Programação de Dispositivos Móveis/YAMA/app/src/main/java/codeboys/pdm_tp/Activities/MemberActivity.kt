package codeboys.pdm_tp.Activities

import android.content.Intent
import android.os.Bundle
import android.view.View
import android.widget.AdapterView
import android.widget.ListAdapter
import android.widget.SimpleAdapter
import androidx.appcompat.app.AppCompatActivity
import androidx.lifecycle.Observer
import androidx.lifecycle.ViewModelProviders
import codeboys.pdm_tp.Model.Member
import codeboys.pdm_tp.R
import codeboys.pdm_tp.ViewModel.MemberViewModel
import kotlinx.android.synthetic.main.activity_member.*
import kotlinx.android.synthetic.main.fragment_team.*


class MemberActivity : AppCompatActivity() {

    private val membersModel by lazy {
        ViewModelProviders
            .of(this)
            .get(MemberViewModel::class.java)
    }
    var idTeam = 0

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_member)
        val actionBar = supportActionBar
        actionBar!!.title = "Members"
        idTeam = intent.getIntExtra("idTeam",0)
        membersModel.getMembers(idTeam).observe(this, Observer<List<Member>> {
            membersListView.adapter = buildAdapter(it)

            membersListView.onItemClickListener =
                    AdapterView.OnItemClickListener { parent, view, position, id ->
                        val intentChat = Intent(this, ChatActivity::class.java)
                        intentChat.putExtra("idTeam", idTeam)
                        startActivity(intentChat)
                    }
            }
        )
    }
    fun goToChat(view: View){
        val intent = Intent(this, ChatActivity::class.java)
        intent.putExtra("idTeam", idTeam)
        startActivity(intent)
    }

    private fun buildAdapter(members: List<Member>): ListAdapter = SimpleAdapter(
        this,
        members.map {
                members -> mapOf(
            "login" to members.login,
            "id" to members.id
        )
        },
        android.R.layout.simple_list_item_2,
        arrayOf("login","id"),
        intArrayOf(android.R.id.text1,android.R.id.text2)
    )

    override fun onStart() {
        super.onStart()
    }

    override fun onResume() {
        super.onResume()
    }
}
