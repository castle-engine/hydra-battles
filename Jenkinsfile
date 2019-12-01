/* -*- mode: groovy -*-
  Confgure how to run our job in Jenkins.
  See https://github.com/castle-engine/castle-engine/wiki/Cloud-Builds-(Jenkins) .
*/

pipeline {
  triggers {
    pollSCM('H/4 * * * *')
    upstream(upstreamProjects: 'castle_game_engine_update_docker_image', threshold: hudson.model.Result.SUCCESS)
  }
  agent {
    docker {
      image 'kambi/castle-engine-cloud-builds-tools:cge-unstable'
    }
  }
  stages {
    stage('Build Desktop') {
      steps {
        sh 'rm -f hydra_battles-*.tar.gz hydra_battles-*.zip hydra_battles*.apk'
        sh 'castle-engine package --os=win64 --cpu=x86_64 --verbose'
        sh 'castle-engine package --os=win32 --cpu=i386 --verbose'
        sh 'castle-engine package --os=linux --cpu=x86_64 --verbose'
      }
    }
    stage('Build Mobile') {
      steps {
        withCredentials([
          file(credentialsId: 'android-cat-astrophe-games-keystore', variable: 'android_cat_astrophe_games_keystore'),
          string(credentialsId: 'android-cat-astrophe-games-keystore-alias', variable: 'android_cat_astrophe_games_keystore_alias'),
          string(credentialsId: 'android-cat-astrophe-games-keystore-alias-password', variable: 'android_cat_astrophe_games_keystore_alias_password'),
          string(credentialsId: 'android-cat-astrophe-games-keystore-store-password', variable: 'android_cat_astrophe_games_keystore_store_password')
        ]) {
          sh '''
          echo "key.store=${android_cat_astrophe_games_keystore}" > AndroidSigningProperties.txt
          echo "key.alias=${android_cat_astrophe_games_keystore_alias}" >> AndroidSigningProperties.txt
          echo "key.store.password=${android_cat_astrophe_games_keystore_store_password}" >> AndroidSigningProperties.txt
          echo "key.alias.password=${android_cat_astrophe_games_keystore_alias_password}" >> AndroidSigningProperties.txt
          '''
          sh 'castle-engine package --target=android --verbose'
        }
      }
    }
  }
  post {
    success {
      archiveArtifacts artifacts: 'hydra_battles*.tar.gz,hydra_battles*.zip,hydra_battles*.apk'
    }
    regression {
      mail to: 'michalis.kambi@gmail.com',
        subject: "[jenkins] Build started failing: ${currentBuild.fullDisplayName}",
        body: "See the build details on ${env.BUILD_URL}"
    }
    failure {
      mail to: 'michalis.kambi@gmail.com',
        subject: "[jenkins] Build failed: ${currentBuild.fullDisplayName}",
        body: "See the build details on ${env.BUILD_URL}"
    }
    fixed {
      mail to: 'michalis.kambi@gmail.com',
        subject: "[jenkins] Build is again successfull: ${currentBuild.fullDisplayName}",
        body: "See the build details on ${env.BUILD_URL}"
    }
  }
}
