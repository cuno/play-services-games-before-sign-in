package com.google.android.gms.games.achievement;

import android.os.Bundle;

import java.util.Iterator;

/**
 * Non-final replacement for mocking purposes. I was unable to use PowerMockito with ScalaTest.
 */
public class AchievementBuffer implements com.google.android.gms.common.data.DataBuffer<com.google.android.gms.games.achievement.Achievement> {

    @Override
    public int getCount() {
        return 0;
    }

    @Override
    public Achievement get(int i) {
        return null;
    }

    @Override
    public Bundle zziu() {
        return null;
    }

    @Override
    public void close() {

    }

    @Override
    public boolean isClosed() {
        return false;
    }

    @Override
    public Iterator<Achievement> iterator() {
        return null;
    }

    @Override
    public Iterator<Achievement> singleRefIterator() {
        return null;
    }

    @Override
    public void release() {

    }
}