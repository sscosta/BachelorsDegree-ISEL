package codeboys.pdm_tp.ViewModel

import android.app.Application
import android.content.ContentValues.TAG
import android.util.Log
import android.widget.Toast
import androidx.lifecycle.AndroidViewModel
import androidx.lifecycle.MutableLiveData
import codeboys.pdm_tp.Model.MessageSummary
import codeboys.pdm_tp.messageList
import codeboys.pdm_tp.repository
import com.google.firebase.firestore.FirebaseFirestore
import java.util.*

class ChatViewModel(val app : Application) : AndroidViewModel(app){

    var idTeam =0

    val messages =  MutableLiveData<List<MessageSummary>>()

    val db = FirebaseFirestore.getInstance()

    val masterMessagesCollection = db.collection("messages")

    val TeamDocument by lazy { masterMessagesCollection.document("team"+idTeam) }

    val teamChatCollection by lazy { TeamDocument.collection("chats") }

    fun setListener(idTeam: Int)
    {
        this.idTeam=idTeam
        teamChatCollection.addSnapshotListener{ doc, ex ->
            if(ex!=null) {
                Log.w("Yama chat", "Listen failed:", ex)
            }
            else if(doc != null){
                loadMessages(idTeam)
            }
        }
        //registerForMessagesChange()
    }

    fun loadMessages(idTeamLoad:Int) {
        teamChatCollection.get()
                .addOnCompleteListener { task ->
                    if (task.isSuccessful) {
                        for (document in task.result!!) {
                            val msg = MessageSummary(
                                    document.get("message").toString(),
                                    document.get("senderId").toString(),
                                    document.get("teamId").toString().toInt(),
                                    document.get("time").toString()
                            )
                            if(!app.messageList.contains(msg))
                                app.messageList.add(msg)
                            Log.d(TAG, document.id + " ====> " + document.data)
                        }
                        messages.value = app.messageList.filter { it.teamId==idTeamLoad }
                    } else {
                        Log.d(TAG, "Error getting documents: ", task.exception)
                    }
                }
    }

    fun postMessage(newMessage: String, idTeam: Int) {
        val msg = MessageSummary(newMessage, app.repository.getProfileSharedData(app)!!.userId,idTeam,Calendar.getInstance().time.toString())
        teamChatCollection.add(
                msg
        ).addOnSuccessListener { documentReference ->
            Toast.makeText(app, "Message sent", Toast.LENGTH_LONG).show()
        }.addOnFailureListener{
            Toast.makeText(app, "FAILED FIRESTORE", Toast.LENGTH_LONG).show()
        }
    }

//    private fun registerForMessagesChange() {
//        val messageReceiver = object : BroadcastReceiver(){
//            override fun onReceive(context: Context, intent: Intent) {
//                val newMsg = getLastMessage()
//
//                val notification = notificationBuilder(context, "New Message")
//                        .setSmallIcon(android.R.drawable.ic_dialog_email)
//                        .setContentTitle("Your chat has a new Message")
//                        .setContentText("$newMsg%")
//                        .build()
//
//                NotificationManagerCompat.from(context)
//                        .notify(8, notification)
//            }
//            private fun getLastMessage(): String {
//                return teamChatCollection.orderBy("time").limit(1).get().toString()
//            }
//        }
//        val messageFilter = IntentFilter(Intent.CATEGORY_ALTERNATIVE)
//
//        registerReceiver(messageReceiver, messageFilter)
//    }
//
//
//
//
//    private fun notificationBuilder(context: Context, channelId: String) : Notification.Builder {
//        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
//            return Notification.Builder(context, channelId)
//        } else {
//            return Notification.Builder(context)
//        }
//    }

}
