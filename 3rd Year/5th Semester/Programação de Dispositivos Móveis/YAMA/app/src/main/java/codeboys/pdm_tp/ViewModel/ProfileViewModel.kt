package codeboys.pdm_tp.ViewModel

import android.app.Application
import android.widget.Toast
import androidx.lifecycle.AndroidViewModel
import androidx.lifecycle.MutableLiveData
import codeboys.pdm_tp.Model.Profile
import codeboys.pdm_tp.repository
import codeboys.pdm_tp.requestQueue

class ProfileViewModel(val app : Application) : AndroidViewModel(app){

    val profile = MutableLiveData<Profile>()
    val profileRepository = app.repository.profileRepository

    init{
        updateProfile()
    }

    private fun updateProfile() {
        profileRepository.loadProfile(
                app.requestQueue,
                {profile.value = it},
                { Toast.makeText(app,"Error on Update Teams: $it", Toast.LENGTH_LONG).show()}
        )
    }
}