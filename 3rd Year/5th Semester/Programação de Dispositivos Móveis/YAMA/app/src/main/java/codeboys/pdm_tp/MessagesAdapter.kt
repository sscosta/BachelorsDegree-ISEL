package codeboys.pdm_tp

import android.util.Log
import android.view.LayoutInflater
import android.view.ViewGroup
import android.widget.TextView
import android.widget.Toast
import androidx.core.content.ContextCompat
import androidx.recyclerview.widget.RecyclerView
import codeboys.pdm_tp.Model.MessageSummary

class MessageViewHolder(val view: ViewGroup) : RecyclerView.ViewHolder(view) {
    private val messageTextView = view.findViewById<TextView>(android.R.id.text1)
    private val messageIdView = view.findViewById<TextView>(android.R.id.text2)

    fun bindTo(message: MessageSummary) {
        Log.d("ADAPTER","inside bindTo for " + message.message)
        messageTextView.text = message.message
        messageIdView.text = message.senderId
    }

    private fun getColor(name: String) =
            if (name[0] == 'A') android.R.color.holo_red_dark else android.R.color.black
}

class MessagesAdapter(val messages: List<MessageSummary>) : RecyclerView.Adapter<MessageViewHolder>() {
    override fun getItemCount(): Int = messages.size

    override fun onCreateViewHolder(parent: ViewGroup, viewType: Int): MessageViewHolder =
            MessageViewHolder(
                    LayoutInflater
                            .from(parent.context)
                            .inflate(android.R.layout.simple_list_item_2, parent, false) as ViewGroup
            )

    override fun onBindViewHolder(holder: MessageViewHolder, position: Int) {
       holder.bindTo(messages[position])
    }
}