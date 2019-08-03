package codeboys.pdm_tp.Repository


import android.content.Context
import android.content.Context.MODE_PRIVATE
import android.widget.Toast
import androidx.lifecycle.MutableLiveData
import codeboys.pdm_tp.MainRepository
import codeboys.pdm_tp.MessagingApp
import codeboys.pdm_tp.Model.Login
import codeboys.pdm_tp.Model.MessageSummary

class Repository(private val mainRepository: MainRepository){
    val SHARED_PREFERENCES_KEY = "userAuth"
    val USER_ID_KEY = "userID"
    val ORGANIZATION_KEY = "organization"
    val TOKEN_KEY = "token"

    val teamRepository = TeamRepository(mainRepository)
    val profileRepository = ProfileRepository(mainRepository)
    val memberRepository = MemberRepository(mainRepository)

    fun setProfileSharedData(context: Context, username : String, organization :String, token : String){
        val userDetails = context.getSharedPreferences(SHARED_PREFERENCES_KEY, MODE_PRIVATE)
        val edit = userDetails.edit()
        edit.putString(USER_ID_KEY, username)
        edit.putString(ORGANIZATION_KEY, organization)
        edit.putString(TOKEN_KEY, token)
        edit.apply()

    }

    var login : Login ? = null
    fun getProfileSharedData(context : Context) : Login? {
        if(login!=null)
            return login
        else{
            login = accessSharedPreferences(context)
            return login
        }
    }

    private fun accessSharedPreferences(context : Context) : Login? {
        val userDetails = context.getSharedPreferences(SHARED_PREFERENCES_KEY, MODE_PRIVATE)
        if (userDetails.getString(USER_ID_KEY, null) == null)
            return null
        else
            return Login(userDetails.getString(USER_ID_KEY, null), userDetails.getString(TOKEN_KEY, null))
    }

}