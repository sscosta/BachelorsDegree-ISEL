package codeboys.pdm_tp.Fragments


import android.os.Bundle
import androidx.fragment.app.Fragment
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import androidx.lifecycle.Observer
import androidx.lifecycle.ViewModelProviders
import codeboys.pdm_tp.Model.Profile

import codeboys.pdm_tp.R
import codeboys.pdm_tp.ViewModel.ProfileViewModel
import kotlinx.android.synthetic.main.fragment_profile.*

class ProfileFragment : Fragment() {

    private val profileModel by lazy {
        ViewModelProviders
                .of(this)
                .get(ProfileViewModel::class.java)
    }


    override fun onCreateView(
        inflater: LayoutInflater, container: ViewGroup?,
        savedInstanceState: Bundle?
        ): View? {

        profileModel.profile.observe(this, Observer<Profile> {
            update(it)
            }
        )
        return inflater.inflate(R.layout.fragment_profile, container, false)
    }

    private fun update(profile : Profile){
        loginTextView.text=profile.login
        nameTextView.text=profile.name
        emailTextView.text=if (profile.email.isNullOrEmpty()) profile.email else "NO EMAIL"
        followersTextView.text=profile.followers.toString()
        avatarTextView.text=profile.avatar_url
    }
}
