package codeboys.pdm_tp.Activities

import androidx.appcompat.app.AppCompatActivity
import android.os.Bundle
import android.view.View
import androidx.lifecycle.Observer
import androidx.lifecycle.ViewModelProviders
import androidx.recyclerview.widget.LinearLayoutManager
import codeboys.pdm_tp.MessagesAdapter
import codeboys.pdm_tp.Model.MessageSummary
import codeboys.pdm_tp.R
import codeboys.pdm_tp.ViewModel.ChatViewModel
import kotlinx.android.synthetic.main.activity_chat.*

class ChatActivity : AppCompatActivity() {

    var idTeam = 0
    private val chatModel by lazy {
        ViewModelProviders
            .of(this)
            .get(ChatViewModel::class.java)
    }

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_chat)

        idTeam= intent.getIntExtra("idTeam",0)
        //chatModel.idTeam=idTeam;


        chatRecyclerView.setHasFixedSize(true)
        chatRecyclerView.layoutManager = LinearLayoutManager(this)
        chatModel.messages.observe(this, Observer<List<MessageSummary>> {
            chatRecyclerView.adapter = MessagesAdapter(it)
        })
        chatModel.setListener(idTeam)

    }

    fun sendMessage(view: View) {
        chatModel.postMessage(edittext_message.text.toString(), idTeam)
        edittext_message.text.clear()
    }


}
