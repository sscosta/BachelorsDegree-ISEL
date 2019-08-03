package codeboys.pdm_tp.Activities

import android.content.Intent
import androidx.appcompat.app.AppCompatActivity
import android.os.Bundle
import android.view.View
import codeboys.pdm_tp.R
import codeboys.pdm_tp.repository
import kotlinx.android.synthetic.main.activity_login.*

class LoginActivity : AppCompatActivity() {

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_login)

        val actionBar = supportActionBar
        actionBar!!.title = "Login"
    }

    fun saveLogin(view : View){
        application.repository.setProfileSharedData(
                                            this,
                                                    editTextUID.text.toString(),
                                                    editTextOrg.text.toString(),
                                                    editTextToken.text.toString()
        )
        startActivity(Intent(this, MainActivity::class.java))
    }
}
