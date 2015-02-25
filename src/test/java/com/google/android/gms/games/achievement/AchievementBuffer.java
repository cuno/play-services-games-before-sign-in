package com.google.android.gms.games.achievement;

import com.google.android.gms.common.data.DataHolder;

/**
 * Non-final replacement for mocking purposes. I was unable to use PowerMockito with ScalaTest.
 */
public class AchievementBuffer extends com.google.android.gms.common.data.DataBuffer<com.google.android.gms.games.achievement.Achievement> {

    protected AchievementBuffer(DataHolder dataHolder) {
        super(dataHolder);
    }

    public com.google.android.gms.games.achievement.Achievement get(int position) {
        return null;
    }
}