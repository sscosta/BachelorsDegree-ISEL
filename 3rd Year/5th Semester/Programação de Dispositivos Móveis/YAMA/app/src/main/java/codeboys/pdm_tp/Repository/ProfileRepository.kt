package codeboys.pdm_tp.Repository

import codeboys.pdm_tp.MainRepository
import codeboys.pdm_tp.MessagingApp
import codeboys.pdm_tp.Model.Profile
import com.android.volley.Request
import com.android.volley.RequestQueue
import com.android.volley.Response
import com.android.volley.toolbox.StringRequest
import org.json.JSONObject

class ProfileRepository(private val mainRepository: MainRepository){

    private val urlProfile = "https://api.github.com/user"
    fun loadProfile(queue: RequestQueue, onSuccess : (Profile) -> Unit, onError : (String) -> Unit) {

        val stringRequest = object : StringRequest(
            Request.Method.GET,
            urlProfile,
            Response.Listener { response ->
                onSuccess(convertProfile(response))
            }, Response.ErrorListener { error ->
                onError(error.message ?: "error" )
            }) {
            override fun getHeaders(): MutableMap<String, String> {
                return mutableMapOf("Authorization" to (mainRepository as MessagingApp).repository.getProfileSharedData(mainRepository)!!.token)
            }
        }
        queue.add(stringRequest)
    }

    private fun convertProfile(response: String): Profile {
        var profileResponse = JSONObject(response)

        return Profile(
                        profileResponse.getString("login"),
                        profileResponse.getString("name"),
                        profileResponse.getString("email"),
                        profileResponse.getInt("followers"),
                        profileResponse.getString("avatar_url")
        )
    }

}